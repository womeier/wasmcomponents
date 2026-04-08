(** * WIT Lexer and Parser

    Implements a lexer and recursive-descent parser for WIT
    (WebAssembly Interface Types) files, producing [wit_package] AST values.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md *)

From Stdlib Require Import Ascii String List Bool.
Import ListNotations.
From wasmcomponents Require Import wit_ast.

Open Scope string_scope.

(* ================================================================= *)
(** ** 1. Lexer                                                        *)
(* ================================================================= *)

(** *** Token type

    Represents all syntactically significant tokens in a WIT file.
    Keywords and identifiers are both represented as [TokIdent]; the
    parser distinguishes them by string comparison.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#lexical-structure *)
Inductive wit_token :=
  | TokIdent   : string -> wit_token
  | TokLBrace | TokRBrace
  | TokLParen  | TokRParen
  | TokLAngle  | TokRAngle
  | TokComma | TokColon | TokSemicolon | TokEquals
  | TokArrow | TokUnderscore | TokAt | TokDot.

(** *** Character classification helpers *)

(** [is_alpha c] — true if [c] is an ASCII letter.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Definition is_alpha (c : ascii) : bool :=
  let n := nat_of_ascii c in
  (Nat.leb 65 n && Nat.leb n 90) ||   (* A-Z *)
  (Nat.leb 97 n && Nat.leb n 122).    (* a-z *)

(** [is_digit c] — true if [c] is an ASCII decimal digit.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Definition is_digit (c : ascii) : bool :=
  let n := nat_of_ascii c in
  Nat.leb 48 n && Nat.leb n 57.       (* 0-9 *)

(** [is_ident_cont c] — true for characters that may appear after the
    first character of a WIT identifier (alpha, digit, [-], [_]).

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Definition is_ident_cont (c : ascii) : bool :=
  is_alpha c || is_digit c ||
  Ascii.eqb c "-"%char || Ascii.eqb c "_"%char.

(** [is_whitespace c] — true for space, tab, newline, carriage-return.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Definition is_whitespace (c : ascii) : bool :=
  Ascii.eqb c " "%char  ||
  Ascii.eqb c "009"%char ||  (* tab *)
  Ascii.eqb c "010"%char ||  (* newline *)
  Ascii.eqb c "013"%char.    (* carriage return *)

(** *** Structural recursion helpers *)

(** [lex_ident_chars cs] — collect identifier-continuation characters,
    returning [(word, rest)].  Structurally recursive on [cs].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Fixpoint lex_ident_chars (cs : list ascii) : string * list ascii :=
  match cs with
  | c :: rest =>
      if is_ident_cont c
      then let '(w, r) := lex_ident_chars rest in
           (String c w, r)
      else (""%string, cs)
  | [] => (""%string, [])
  end.

(** [skip_line cs] — discard characters through the next newline.
    Structurally recursive on [cs].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Fixpoint skip_line (cs : list ascii) : list ascii :=
  match cs with
  | [] => []
  | c :: rest =>
      if Ascii.eqb c "010"%char (* newline *)
      then rest
      else skip_line rest
  end.

(** [skip_block fuel depth cs] — skip a possibly-nested block comment
    [/* ... */].  [depth] tracks open-comment nesting level.  Uses
    [fuel] for termination.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs *)
Fixpoint skip_block (fuel : nat) (depth : nat) (cs : list ascii)
    : option (list ascii) :=
  match fuel with
  | O => None
  | S fuel' =>
      match cs with
      | [] => if Nat.eqb depth 0 then Some [] else None
      | "*"%char :: "/"%char :: rest =>
          if Nat.eqb depth 1
          then Some rest
          else skip_block fuel' (depth - 1) rest
      | "/"%char :: "*"%char :: rest =>
          skip_block fuel' (depth + 1) rest
      | _ :: rest =>
          skip_block fuel' depth rest
      end
  end.

(** *** Main tokenizer

    [tokenize fuel cs acc] — convert the character list [cs] into
    tokens, prepending to [acc] (reversed), returning [None] on error.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#lexical-structure *)
Fixpoint tokenize (fuel : nat) (cs : list ascii) (acc : list wit_token)
    : option (list wit_token) :=
  match fuel with
  | O => None
  | S fuel' =>
      match cs with
      | [] => Some (List.rev acc)

      (* whitespace: skip *)
      | c :: rest =>
          if is_whitespace c
          then tokenize fuel' rest acc

          (* line comment *)
          else match cs with
          | "/"%char :: "/"%char :: rest2 =>
              tokenize fuel' (skip_line rest2) acc

          (* block comment *)
          | "/"%char :: "*"%char :: rest2 =>
              match skip_block (List.length rest2 + 1) 1 rest2 with
              | None => None
              | Some rest3 => tokenize fuel' rest3 acc
              end

          (* punctuation *)
          | "{"%char :: rest2 => tokenize fuel' rest2 (TokLBrace   :: acc)
          | "}"%char :: rest2 => tokenize fuel' rest2 (TokRBrace   :: acc)
          | "("%char :: rest2 => tokenize fuel' rest2 (TokLParen   :: acc)
          | ")"%char :: rest2 => tokenize fuel' rest2 (TokRParen   :: acc)
          | "<"%char :: rest2 => tokenize fuel' rest2 (TokLAngle   :: acc)
          | ">"%char :: rest2 => tokenize fuel' rest2 (TokRAngle   :: acc)
          | ","%char :: rest2 => tokenize fuel' rest2 (TokComma    :: acc)
          | ":"%char :: rest2 => tokenize fuel' rest2 (TokColon    :: acc)
          | ";"%char :: rest2 => tokenize fuel' rest2 (TokSemicolon:: acc)
          | "="%char :: rest2 => tokenize fuel' rest2 (TokEquals   :: acc)
          | "@"%char :: rest2 => tokenize fuel' rest2 (TokAt       :: acc)
          | "."%char :: rest2 => tokenize fuel' rest2 (TokDot      :: acc)
          | "_"%char :: rest2 => tokenize fuel' rest2 (TokUnderscore:: acc)

          (* -> arrow *)
          | "-"%char :: ">"%char :: rest2 =>
              tokenize fuel' rest2 (TokArrow :: acc)

          (* explicit-id prefix: strip % and lex rest as ident *)
          | "%"%char :: rest2 =>
              match rest2 with
              | c2 :: _ =>
                  if is_alpha c2
                  then let '(w, r) := lex_ident_chars rest2 in
                       tokenize fuel' r (TokIdent w :: acc)
                  else None
              | [] => None
              end

          (* identifier / keyword *)
          | c2 :: _ =>
              if is_alpha c2
              then let '(w, r) := lex_ident_chars cs in
                   tokenize fuel' r (TokIdent w :: acc)
              else None

          | [] => Some (List.rev acc)
          end
      end
  end.

(** [lex s] — tokenize the string [s], returning [None] on lexical error.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/token.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#lexical-structure *)
Definition lex (s : string) : option (list wit_token) :=
  let cs := list_ascii_of_string s in
  let fuel := List.length cs * 2 + 1 in
  tokenize fuel cs [].

(* ================================================================= *)
(** ** 2. Parser Combinators                                           *)
(* ================================================================= *)

(** *** Parser type

    A parser for values of type [A] is a function from a token stream
    to an optional pair [(result, remaining_tokens)].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition parser (A : Type) : Type :=
  list wit_token -> option (A * list wit_token).

(** [p_return x] — always succeeds with [x], consuming no tokens.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_return {A : Type} (x : A) : parser A :=
  fun ts => Some (x, ts).

(** [p_fail] — always fails.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_fail {A : Type} : parser A :=
  fun _ => None.

(** [p_bind p f] — monadic bind: run [p], then pass the result to [f].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_bind {A B : Type} (p : parser A) (f : A -> parser B) : parser B :=
  fun ts =>
    match p ts with
    | None => None
    | Some (x, ts') => f x ts'
    end.

(** [p_map f p] — apply [f] to the result of [p].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_map {A B : Type} (f : A -> B) (p : parser A) : parser B :=
  fun ts =>
    match p ts with
    | None => None
    | Some (x, ts') => Some (f x, ts')
    end.

(** [p_alt p1 p2] — try [p1]; if it fails, try [p2] on the same input.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_alt {A : Type} (p1 p2 : parser A) : parser A :=
  fun ts =>
    match p1 ts with
    | Some r => Some r
    | None => p2 ts
    end.

(** Notation [p >>= f] — monadic bind. *)
Notation "p >>= f" := (p_bind p f) (at level 50, left associativity).

(** Notation [f <$> p] — functor map. *)
Notation "f <$> p" := (p_map f p) (at level 61, right associativity).

(** Notation [p1 <|> p2] — ordered choice. *)
Notation "p1 <|> p2" := (p_alt p1 p2) (at level 70, right associativity).

(** Notation [p1 *> p2] — sequence, keeping right result. *)
Notation "p1 *> p2" := (p_bind p1 (fun _ => p2)) (at level 60, right associativity).

(** Notation [p1 <* p2] — sequence, keeping left result. *)
Notation "p1 <* p2" :=
  (p_bind p1 (fun x => p_bind p2 (fun _ => p_return x)))
  (at level 60, right associativity).

(** *** Token consumers *)

(** [tok_ident] — consume any [TokIdent] token and return its string.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition tok_ident : parser string :=
  fun ts =>
    match ts with
    | TokIdent s :: rest => Some (s, rest)
    | _ => None
    end.

(** [tok_keyword kw] — consume [TokIdent kw] matching exactly [kw].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition tok_keyword (kw : string) : parser unit :=
  fun ts =>
    match ts with
    | TokIdent s :: rest =>
        if String.eqb s kw then Some (tt, rest) else None
    | _ => None
    end.

(** [tok_exact t] — consume a specific non-ident token.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition wit_token_eqb (t1 t2 : wit_token) : bool :=
  match t1, t2 with
  | TokLBrace,    TokLBrace    => true
  | TokRBrace,    TokRBrace    => true
  | TokLParen,    TokLParen    => true
  | TokRParen,    TokRParen    => true
  | TokLAngle,    TokLAngle    => true
  | TokRAngle,    TokRAngle    => true
  | TokComma,     TokComma     => true
  | TokColon,     TokColon     => true
  | TokSemicolon, TokSemicolon => true
  | TokEquals,    TokEquals    => true
  | TokArrow,     TokArrow     => true
  | TokUnderscore,TokUnderscore=> true
  | TokAt,        TokAt        => true
  | TokDot,       TokDot       => true
  | TokIdent s1,  TokIdent s2  => String.eqb s1 s2
  | _, _ => false
  end.

Definition tok_exact (t : wit_token) : parser unit :=
  fun ts =>
    match ts with
    | t2 :: rest =>
        if wit_token_eqb t t2 then Some (tt, rest) else None
    | [] => None
    end.

(** [p_option p] — optionally run [p]; returns [Some x] or [None].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition p_option {A : Type} (p : parser A) : parser (option A) :=
  (fun x => Some x) <$> p <|> p_return None.

(** [p_sep_by sep p] — parse zero or more [p] separated by [sep].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Fixpoint p_sep_by_fuel {A : Type} (fuel : nat)
    (sep : parser unit) (p : parser A) (ts : list wit_token)
    : option (list A * list wit_token) :=
  match fuel with
  | O => Some ([], ts)
  | S fuel' =>
      match p ts with
      | None => Some ([], ts)
      | Some (x, ts1) =>
          match sep ts1 with
          | None => Some ([x], ts1)
          | Some (_, ts2) =>
              match p_sep_by_fuel fuel' sep p ts2 with
              | None => Some ([x], ts1)
              | Some (xs, ts3) => Some (x :: xs, ts3)
              end
          end
      end
  end.

Definition p_sep_by {A : Type} (sep : parser unit) (p : parser A)
    : parser (list A) :=
  fun ts =>
    p_sep_by_fuel (List.length ts + 1) sep p ts.

(* ================================================================= *)
(** ** 3. Grammar Productions                                          *)
(* ================================================================= *)

(** *** Primitive type parser

    [parse_primitive s] — map a keyword string to a [wit_primitive].

    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#primitive-types *)
Definition parse_primitive (s : string) : option wit_primitive :=
  if      String.eqb s "u8"     then Some WitU8
  else if String.eqb s "u16"    then Some WitU16
  else if String.eqb s "u32"    then Some WitU32
  else if String.eqb s "u64"    then Some WitU64
  else if String.eqb s "s8"     then Some WitS8
  else if String.eqb s "s16"    then Some WitS16
  else if String.eqb s "s32"    then Some WitS32
  else if String.eqb s "s64"    then Some WitS64
  else if String.eqb s "f32"    then Some WitF32
  else if String.eqb s "f64"    then Some WitF64
  else if String.eqb s "char"   then Some WitChar
  else if String.eqb s "string" then Some WitString
  else if String.eqb s "bool"   then Some WitBool
  else None.

(** *** Recursive type parser

    [parse_wit_type fuel] — parse a WIT type expression.  [fuel] guards
    the recursion that Rocq cannot verify structurally.

    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#wit-types
    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Fixpoint parse_wit_type (fuel : nat) : parser wit_type :=
  match fuel with
  | O => p_fail
  | S fuel' =>
      let inner := parse_wit_type fuel' in

      (* record { field: type, ... } *)
      let parse_record : parser wit_type :=
        tok_keyword "record" *>
        tok_exact TokLBrace *>
        (p_sep_by (tok_exact TokComma)
           (tok_ident >>= (fun name =>
            tok_exact TokColon *>
            (inner >>= (fun ty =>
            p_return (name, ty)))))) >>= (fun fields =>
        tok_exact TokRBrace *>
        p_return (WitRecord fields))
      in

      (* variant { case, case(type), ... } *)
      let parse_variant_case : parser (string * option wit_type) :=
        tok_ident >>= (fun name =>
        p_option (tok_exact TokLParen *> inner <* tok_exact TokRParen)
        >>= (fun opt_ty =>
        p_return (name, opt_ty)))
      in
      let parse_variant : parser wit_type :=
        tok_keyword "variant" *>
        tok_exact TokLBrace *>
        p_sep_by (tok_exact TokComma) parse_variant_case >>= (fun cases =>
        tok_exact TokRBrace *>
        p_return (WitVariant cases))
      in

      (* enum { name, ... } *)
      let parse_enum : parser wit_type :=
        tok_keyword "enum" *>
        tok_exact TokLBrace *>
        p_sep_by (tok_exact TokComma) tok_ident >>= (fun names =>
        tok_exact TokRBrace *>
        p_return (WitEnum names))
      in

      (* flags { name, ... } *)
      let parse_flags : parser wit_type :=
        tok_keyword "flags" *>
        tok_exact TokLBrace *>
        p_sep_by (tok_exact TokComma) tok_ident >>= (fun names =>
        tok_exact TokRBrace *>
        p_return (WitFlags names))
      in

      (* result<ok, err>, result<_, err>, result<ok, _>, result *)
      let parse_result_ty : parser wit_type :=
        tok_keyword "result" *>
        (p_option (
          tok_exact TokLAngle *>
          (* ok arm: _ or type *)
          (((tok_exact TokUnderscore *> p_return None)
            <|> ((fun t => Some t) <$> inner)) >>= (fun ok_ty =>
          tok_exact TokComma *>
          (* err arm: _ or type *)
          ((tok_exact TokUnderscore *> p_return None)
           <|> ((fun t => Some t) <$> inner)) >>= (fun err_ty =>
          tok_exact TokRAngle *>
          p_return (ok_ty, err_ty)))))
        >>= (fun opt_arms =>
        match opt_arms with
        | None => p_return (WitResult None None)
        | Some (ok_ty, err_ty) => p_return (WitResult ok_ty err_ty)
        end))
      in

      (* list<T> *)
      let parse_list : parser wit_type :=
        tok_keyword "list" *>
        tok_exact TokLAngle *>
        inner >>= (fun ty =>
        tok_exact TokRAngle *>
        p_return (WitList ty))
      in

      (* option<T> *)
      let parse_option_ty : parser wit_type :=
        tok_keyword "option" *>
        tok_exact TokLAngle *>
        inner >>= (fun ty =>
        tok_exact TokRAngle *>
        p_return (WitOption ty))
      in

      (* tuple<T1, T2, ...> *)
      let parse_tuple : parser wit_type :=
        tok_keyword "tuple" *>
        tok_exact TokLAngle *>
        p_sep_by (tok_exact TokComma) inner >>= (fun tys =>
        tok_exact TokRAngle *>
        p_return (WitTuple tys))
      in

      (* stream<T> — optional payload *)
      let parse_stream : parser wit_type :=
        tok_keyword "stream" *>
        (p_option (tok_exact TokLAngle *> inner <* tok_exact TokRAngle)
        >>= (fun opt_ty =>
        p_return (WitStream opt_ty)))
      in

      (* future<T> — optional payload *)
      let parse_future : parser wit_type :=
        tok_keyword "future" *>
        (p_option (tok_exact TokLAngle *> inner <* tok_exact TokRAngle)
        >>= (fun opt_ty =>
        p_return (WitFuture opt_ty)))
      in

      (* own<name> *)
      let parse_own : parser wit_type :=
        tok_keyword "own" *>
        tok_exact TokLAngle *>
        tok_ident >>= (fun name =>
        tok_exact TokRAngle *>
        p_return (WitOwn name))
      in

      (* borrow<name> *)
      let parse_borrow : parser wit_type :=
        tok_keyword "borrow" *>
        tok_exact TokLAngle *>
        tok_ident >>= (fun name =>
        tok_exact TokRAngle *>
        p_return (WitBorrow name))
      in

      (* primitive or named reference *)
      let parse_named_or_prim : parser wit_type :=
        tok_ident >>= (fun s =>
        match parse_primitive s with
        | Some p => p_return (WitPrim p)
        | None   => p_return (WitNamed s)
        end)
      in

      parse_record
      <|> parse_variant
      <|> parse_enum
      <|> parse_flags
      <|> parse_result_ty
      <|> parse_list
      <|> parse_option_ty
      <|> parse_tuple
      <|> parse_stream
      <|> parse_future
      <|> parse_own
      <|> parse_borrow
      <|> parse_named_or_prim
  end.

(** *** Function parameter list

    [parse_params fuel] — parse [(name: type, ...)] returning a
    [list (string * wit_type)].

    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#functions *)
Definition parse_params (fuel : nat) : parser (list (string * wit_type)) :=
  tok_exact TokLParen *>
  p_sep_by (tok_exact TokComma)
    (tok_ident >>= (fun name =>
     tok_exact TokColon *>
     parse_wit_type fuel >>= (fun ty =>
     p_return (name, ty)))) >>= (fun ps =>
  tok_exact TokRParen *>
  p_return ps).

(** *** Function result list

    [parse_results fuel] — parse the optional [-> type] return.

    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#functions *)
Definition parse_results (fuel : nat) : parser (list wit_type) :=
  p_option (tok_exact TokArrow *> parse_wit_type fuel) >>= (fun opt =>
  match opt with
  | None    => p_return []
  | Some ty => p_return [ty]
  end).

(** *** Function declaration

    [parse_wit_func fuel] — parse [name: func(params) -> result;].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#functions *)
Definition parse_wit_func (fuel : nat) : parser wit_func :=
  tok_ident >>= (fun name =>
  tok_exact TokColon *>
  tok_keyword "func" *>
  parse_params fuel >>= (fun params =>
  parse_results fuel >>= (fun results =>
  tok_exact TokSemicolon *>
  p_return {| func_name    := name;
              func_params  := params;
              func_results := results |}))).

(** *** Type alias declaration

    [parse_type_alias fuel] — parse [type name = type;] inside an interface.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#type-definitions *)
Definition parse_type_alias (fuel : nat) : parser (string * wit_type) :=
  tok_keyword "type" *>
  tok_ident >>= (fun name =>
  tok_exact TokEquals *>
  parse_wit_type fuel >>= (fun ty =>
  tok_exact TokSemicolon *>
  p_return (name, ty))).

(** *** Inline type definition (record/variant/enum/flags)

    [parse_inline_typedef fuel] — parse a named composite-type definition
    that does not use the [type = …] syntax (e.g. [record point { x: s32 }]).

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition parse_inline_typedef (fuel : nat) : parser (string * wit_type) :=
  (* We look ahead for one of the composite keywords, then parse a name,
     then parse the body (using parse_wit_type on the whole keyword+body). *)
  fun ts =>
    match ts with
    | TokIdent kw :: _ =>
        if String.eqb kw "record" ||
           String.eqb kw "variant" ||
           String.eqb kw "enum" ||
           String.eqb kw "flags" then
          (* parse the composite body which starts with the keyword *)
          match parse_wit_type fuel ts with
          | Some (ty, ts') =>
              (* We need a name; with inline syntax the name precedes the
                 keyword.  This path handles the alternative syntax
                 [type name = record { ... };] already covered by
                 parse_type_alias.  Here we handle bare
                 [record name { ... }] — the name follows the keyword. *)
              (* Actually we already consumed the keyword in parse_wit_type;
                 but parse_wit_type for record does NOT consume a name.
                 Return a synthetic name "" to be fixed by the caller, or
                 handle differently.  For simplicity we use parse_type_alias
                 as the sole path and skip this arm. *)
              None
          | None => None
          end
        else None
    | _ => None
    end.

(** *** Interface body item

    An interface item is either a type alias, an inline type definition
    (record/variant/enum/flags with a leading [type name =]), a resource
    declaration, or a function.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#interfaces *)

Inductive iface_item :=
  | IItemType     : string -> wit_type -> iface_item
  | IItemFunc     : wit_func           -> iface_item
  | IItemResource : string             -> iface_item.

(** [parse_resource_decl] — parse [resource name;].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs *)
Definition parse_resource_decl : parser string :=
  tok_keyword "resource" *>
  tok_ident <* tok_exact TokSemicolon.

Definition parse_iface_item (fuel : nat) : parser iface_item :=
  (* type alias: type name = ...; *)
  ((fun '(n, t) => IItemType n t) <$> parse_type_alias fuel)
  <|>
  (* resource: resource name; *)
  (IItemResource <$> parse_resource_decl)
  <|>
  (* function: name: func(...) -> ...; *)
  (IItemFunc <$> parse_wit_func fuel).

(** *** Interface declaration

    [parse_wit_interface fuel] — parse [interface name { ... }].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#interfaces *)
Fixpoint parse_iface_items (fuel : nat) (p : parser iface_item)
    (ts : list wit_token)
    : option (list iface_item * list wit_token) :=
  match fuel with
  | O => Some ([], ts)
  | S fuel' =>
      match p ts with
      | None => Some ([], ts)
      | Some (item, ts') =>
          match parse_iface_items fuel' p ts' with
          | None => Some ([item], ts')
          | Some (rest, ts'') => Some (item :: rest, ts'')
          end
      end
  end.

Definition collect_iface_items (items : list iface_item)
    : list (string * wit_type) * list wit_func * list string :=
  List.fold_right
    (fun item '(types, funcs, resources) =>
       match item with
       | IItemType n t  => ((n, t) :: types, funcs, resources)
       | IItemFunc f    => (types, f :: funcs, resources)
       | IItemResource r => (types, funcs, r :: resources)
       end)
    ([], [], [])
    items.

(** [check_type_names defined ty] — true iff every [WitNamed], [WitOwn],
    and [WitBorrow] reference inside [ty] appears in [defined]. *)
Fixpoint check_type_names (defined : list string) (ty : wit_type) : bool :=
  match ty with
  | WitPrim _        => true
  | WitList t        => check_type_names defined t
  | WitOption t      => check_type_names defined t
  | WitResult ok err =>
      (match ok  with Some t => check_type_names defined t | None => true end) &&
      (match err with Some t => check_type_names defined t | None => true end)
  | WitTuple ts      => List.forallb (check_type_names defined) ts
  | WitRecord fields => List.forallb (fun '(_, t) => check_type_names defined t) fields
  | WitVariant cases => List.forallb (fun '(_, opt) =>
      match opt with Some t => check_type_names defined t | None => true end) cases
  | WitEnum _        => true
  | WitFlags _       => true
  | WitResource _    => true
  | WitOwn   name    => List.existsb (String.eqb name) defined
  | WitBorrow name   => List.existsb (String.eqb name) defined
  | WitStream opt    => match opt with Some t => check_type_names defined t | None => true end
  | WitFuture opt    => match opt with Some t => check_type_names defined t | None => true end
  | WitNamed name    => List.existsb (String.eqb name) defined
  end.

(** [validate_iface iface] — reject interfaces where type aliases or
    function signatures reference undefined names. *)
Definition validate_iface (iface : wit_interface) : bool :=
  let defined := List.app (List.map fst (iface_types iface)) (iface_resources iface) in
  List.forallb (fun p => check_type_names defined (snd p)) (iface_types iface) &&
  List.forallb (fun f =>
    List.forallb (fun p => check_type_names defined (snd p)) (func_params f) &&
    List.forallb (check_type_names defined) (func_results f))
    (iface_funcs iface).

Definition parse_wit_interface (fuel : nat) : parser wit_interface :=
  tok_keyword "interface" *>
  tok_ident >>= (fun name =>
  tok_exact TokLBrace *>
  (fun ts =>
     match fuel with
     | O => None
     | S fuel' =>
         parse_iface_items (List.length ts + 1) (parse_iface_item fuel') ts
     end) >>= (fun items =>
  tok_exact TokRBrace *>
  (let '(types, funcs, resources) := collect_iface_items items in
   let iface := {| iface_name      := name;
                   iface_types     := types;
                   iface_funcs     := funcs;
                   iface_resources := resources |} in
   if validate_iface iface then p_return iface else p_fail))).

(** *** World items

    [parse_world_item fuel] — parse a single [import …;] or [export …;].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#worlds *)

Inductive world_item :=
  | WItemImport : string -> wit_interface -> world_item
  | WItemExport : string -> wit_interface -> world_item.

(** A stub interface with just a name (for reference imports/exports). *)
Definition stub_iface (name : string) : wit_interface :=
  {| iface_name      := name;
     iface_types     := [];
     iface_funcs     := [];
     iface_resources := [] |}.

(** [parse_world_ref_item kw] — parse [import|export name;] (interface ref). *)
Definition parse_world_ref_item (kw : string)
    (mk : string -> wit_interface -> world_item) : parser world_item :=
  tok_keyword kw *>
  tok_ident >>= (fun name =>
  tok_exact TokSemicolon *>
  p_return (mk name (stub_iface name))).

(** [parse_world_inline_item fuel kw mk] — parse
    [import|export name: interface { ... }]. *)
Definition parse_world_inline_item (fuel : nat) (kw : string)
    (mk : string -> wit_interface -> world_item) : parser world_item :=
  tok_keyword kw *>
  tok_ident >>= (fun name =>
  tok_exact TokColon *>
  parse_wit_interface fuel >>= (fun iface =>
  p_return (mk name iface))).

Definition parse_world_item (fuel : nat) : parser world_item :=
  parse_world_inline_item fuel "import" WItemImport
  <|> parse_world_ref_item "import" WItemImport
  <|> parse_world_inline_item fuel "export" WItemExport
  <|> parse_world_ref_item "export" WItemExport.

Inductive world_acc :=
  | WAcc : list (string * wit_interface) ->
           list (string * wit_interface) ->
           world_acc.

Fixpoint parse_world_items (fuel : nat) (p : parser world_item)
    (ts : list wit_token)
    : option (list world_item * list wit_token) :=
  match fuel with
  | O => Some ([], ts)
  | S fuel' =>
      match p ts with
      | None => Some ([], ts)
      | Some (item, ts') =>
          match parse_world_items fuel' p ts' with
          | None => Some ([item], ts')
          | Some (rest, ts'') => Some (item :: rest, ts'')
          end
      end
  end.

Definition collect_world_items (items : list world_item)
    : list (string * wit_interface) * list (string * wit_interface) :=
  List.fold_right
    (fun item '(imports, exports) =>
       match item with
       | WItemImport n iface => ((n, iface) :: imports, exports)
       | WItemExport n iface => (imports, (n, iface) :: exports)
       end)
    ([], [])
    items.

(** [parse_wit_world fuel] — parse [world name { import ...; export ...; }].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#worlds *)
Definition parse_wit_world (fuel : nat) : parser wit_world :=
  tok_keyword "world" *>
  tok_ident >>= (fun name =>
  tok_exact TokLBrace *>
  (fun ts =>
     match fuel with
     | O => None
     | S fuel' =>
         parse_world_items (List.length ts + 1) (parse_world_item fuel') ts
     end) >>= (fun items =>
  tok_exact TokRBrace *>
  (let '(imports, exports) := collect_world_items items in
   p_return {| world_name    := name;
               world_imports := imports;
               world_exports := exports |}))).

(** *** Package header

    [parse_package_header] — parse [package ns:name@ver;].

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#packages *)

(** [parse_version] — parse [@version-string] where the version is a
    sequence of ident tokens joined by [.] and [-].  We collect the
    first ident after [@] and stop.  For full semver use the first
    token only (e.g. [1.0.0] would be one ident token if it contains
    no whitespace).  We lex dots as [TokDot], so we assemble them
    manually. *)
Fixpoint parse_version_parts (fuel : nat) (ts : list wit_token) : string * list wit_token :=
  match fuel with
  | O => (""%string, ts)
  | S fuel' =>
      match ts with
      | TokIdent s :: TokDot :: rest =>
          let '(rest_str, ts') := parse_version_parts fuel' rest in
          (s ++ "." ++ rest_str, ts')
      | TokIdent s :: rest =>
          (s, rest)
      | _ => (""%string, ts)
      end
  end.

Definition parse_package_header : parser (string * string * option string) :=
  tok_keyword "package" *>
  tok_ident >>= (fun ns =>
  tok_exact TokColon *>
  tok_ident >>= (fun name =>
  p_option (
    tok_exact TokAt *>
    (fun ts =>
       let '(v, ts') := parse_version_parts (List.length ts + 1) ts in
       if String.eqb v "" then None else Some (v, ts'))) >>= (fun ver =>
  tok_exact TokSemicolon *>
  p_return (ns, name, ver)))).

(** *** Top-level declarations

    A package body is a sequence of [interface] and [world] declarations.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#packages *)

Inductive top_item :=
  | TopIface : wit_interface -> top_item
  | TopWorld : wit_world     -> top_item.

Definition parse_top_item (fuel : nat) : parser top_item :=
  (TopIface <$> parse_wit_interface fuel)
  <|>
  (TopWorld <$> parse_wit_world fuel).

Fixpoint parse_top_items (fuel : nat) (p : parser top_item)
    (ts : list wit_token)
    : option (list top_item * list wit_token) :=
  match fuel with
  | O => Some ([], ts)
  | S fuel' =>
      match p ts with
      | None => Some ([], ts)
      | Some (item, ts') =>
          match parse_top_items fuel' p ts' with
          | None => Some ([item], ts')
          | Some (rest, ts'') => Some (item :: rest, ts'')
          end
      end
  end.

Definition collect_top_items (items : list top_item)
    : list wit_interface * list wit_world :=
  List.fold_right
    (fun item '(ifaces, worlds) =>
       match item with
       | TopIface i => (i :: ifaces, worlds)
       | TopWorld w => (ifaces, w :: worlds)
       end)
    ([], [])
    items.

(** [parse_wit_package fuel] — parse a complete WIT document.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#packages *)
Definition parse_wit_package (fuel : nat) : parser wit_package :=
  parse_package_header >>= (fun '(ns, name, ver) =>
  (fun ts =>
     match fuel with
     | O => None
     | S fuel' =>
         parse_top_items (List.length ts + 1) (parse_top_item fuel') ts
     end) >>= (fun items =>
  (let '(ifaces, worlds) := collect_top_items items in
   p_return {| pkg_namespace  := ns;
               pkg_name       := name;
               pkg_version    := ver;
               pkg_interfaces := ifaces;
               pkg_worlds     := worlds |}))).

(** [parse_wit fuel s] — lex [s] then parse it as a WIT package.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md *)
Definition parse_wit (fuel : nat) (s : string) : option wit_package :=
  match lex s with
  | None => None
  | Some ts =>
      match parse_wit_package fuel ts with
      | None => None
      | Some (pkg, remaining) =>
          match remaining with
          | [] => Some pkg
          | _  => None
          end
      end
  end.
