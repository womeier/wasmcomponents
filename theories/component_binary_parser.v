(** * Component binary parser (strict mode).

    This parser handles a strict subset of the WebAssembly component binary
    format. It returns explicit error strings for unsupported constructs and
    malformed payloads.

    Rust: https://github.com/bytecodealliance/wasm-tools
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#binary-format
    *)
From Stdlib Require Import String List NArith ZArith.
From compcert Require Import Integers.
From Wasm Require Import datatypes binary_format_parser pp.
From wasmcomponents Require Import component_ast.

Import ListNotations.
Open Scope N_scope.
Open Scope bool_scope.
Open Scope byte_scope.

Definition component_result (A : Type) : Type := A + string.

Definition component_ok {A} (x : A) : component_result A := inl x.
Definition component_err {A} (msg : string) : component_result A := inr msg.

Definition sid_import_id : byte := Byte.repr (Z.of_nat 10%nat).
Definition sid_export_id : byte := Byte.repr (Z.of_nat 11%nat).
Definition sid_value_id : byte := Byte.repr (Z.of_nat 12%nat).

Definition section_id_name (sid : byte) : string :=
  if byte_eqb sid (#00) then "custom"
  else if byte_eqb sid (#01) then "core_module"
  else if byte_eqb sid (#02) then "core_instance"
  else if byte_eqb sid (#03) then "core_type"
  else if byte_eqb sid (#04) then "component"
  else if byte_eqb sid (#05) then "instance"
  else if byte_eqb sid (#06) then "alias"
  else if byte_eqb sid (#07) then "type"
  else if byte_eqb sid (#08) then "canon"
  else if byte_eqb sid (#09) then "start"
  else if byte_eqb sid sid_import_id then "import"
   else if byte_eqb sid sid_export_id then "export"
   else if byte_eqb sid sid_value_id then "value"
  else "unknown".

Definition byte_to_string (b : byte) : string :=
  section_id_name b.

Fixpoint parse_u32leb_aux (n : nat) (shift : N) (acc : N) (bytes : list byte) : option (N * list byte) :=
  match n with
  | O => None
  | S n' =>
      match bytes with
      | [] => None
      | b :: rest =>
           let v : N := Z.to_N (Byte.unsigned b) in
          let payload : N := N.modulo v 128%N in
           let acc' : N := acc + N.shiftl payload shift in
          if N.ltb v 128 then Some (acc', rest)
          else parse_u32leb_aux n' (shift + 7%N) acc' rest
      end
  end.

Definition parse_u32leb (bs : list byte) : option (N * list byte) :=
  parse_u32leb_aux 5%nat 0%N 0%N bs.

Definition split_bytes (n : N) (bs : list byte) : option (list byte * list byte) :=
  let k := N.to_nat n in
  if Nat.leb k (List.length bs) then
    Some (List.firstn k bs, List.skipn k bs)
  else
    None.

Definition parse_preamble (bs : list byte) : component_result (list byte) :=
  match bs with
  | b0 :: b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: tail =>
      if byte_eqb b0 (Byte.repr 0) && byte_eqb b1 (Byte.repr 97) && byte_eqb b2 (Byte.repr 115) &&
         byte_eqb b3 (Byte.repr 109) && byte_eqb b4 (Byte.repr 13) &&
         byte_eqb b5 (Byte.repr 0) && byte_eqb b6 (Byte.repr 1) && byte_eqb b7 (Byte.repr 0) then
        component_ok tail
      else
        component_err "invalid preamble: expected 00 61 73 6d 0d 00 01 00"
  | _ =>
       component_err "invalid preamble: expected 00 61 73 6d 0d 00 01 00"
   end.

Record parse_state : Type := mk_parse_state
  {
    ps_core_modules : list module;
    ps_core_instances : list core_instance;
    ps_core_types : list core_type;
    ps_components : list component;
    ps_instances : list instance;
    ps_aliases : list alias;
    ps_types : list component_type;
    ps_canons : list canon;
    ps_start : option start;
    ps_imports : list import;
    ps_exports : list export
  }.

Definition empty_parse_state : parse_state :=
  mk_parse_state nil nil nil nil nil nil nil nil None nil nil.

(** Helper: read a u32 LEB128 count from the start of a payload.
    Returns the count and the remaining bytes. *)
Definition parse_payload_count (payload : list byte) : option (N * list byte) :=
  parse_u32leb payload.

(** Helper: create [n] copies of a value. *)
Fixpoint replicate {A : Type} (n : nat) (x : A) : list A :=
  match n with
  | O => nil
  | S n' => x :: replicate n' x
  end.

(** An empty component used as a placeholder for nested components. *)
Definition empty_component : component :=
  Build_component nil nil nil nil nil nil nil nil None nil nil.

Definition parse_core_module_payload (payload : list byte) : component_result (list module) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_module) with
  | Some ms => component_ok (ms :: nil)
  | None => component_err "malformed payload in section 1 (core_module)"
  end.

(** Parse section 02 (core_instance).

    Binary format:
      count : u32 (LEB128)
      For each instance, either:
        0x00 + module_index:u32 + vec(instantiation_arg)  (Instantiate)
        0x01 + vec(export)                                (FromExports)

    Since our AST represents core instances as [moduleinst] (a runtime type
    that cannot be meaningfully constructed at parse time), we store
    [empty_moduleinst] placeholders for each parsed instance.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/instances.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#core-instance-definitions}}
    *)
Definition parse_core_instance_payload (payload : list byte) : component_result (list core_instance) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count) empty_moduleinst)
  | None => component_err "malformed payload in section 2 (core_instance): invalid count"
  end.

Definition parse_core_type_payload (payload : list byte) : component_result (list core_type) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_vec parse_function_type) with
  | Some ts => component_ok ts
  | None => component_err "malformed payload in section 3 (core_type)"
  end.

(** Parse section 05 (instance).

    Component-level instances.  As with core instances the AST type is
    [moduleinst] so we store placeholders.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/instances.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#instance-definitions}}
    *)
Definition parse_instance_payload (payload : list byte) : component_result (list instance) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count) empty_moduleinst)
  | None => component_err "malformed payload in section 5 (instance): invalid count"
  end.

(** Parse section 06 (alias).

    Aliases resolve references across component boundaries.  Since our AST type
    for aliases is just [string], we store placeholder strings.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/aliases.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#aliases}}
    *)
Definition parse_alias_payload (payload : list byte) : component_result (list alias) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count) "alias"%string)
  | None => component_err "malformed payload in section 6 (alias): invalid count"
  end.

(** Parse section 07 (type).

    Component-level type definitions.  The binary encoding is complex
    (recursive types, resource types, etc.) so we skip the payload contents
    and store placeholder types.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/types.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#type-definitions}}
    *)
Definition parse_type_payload (payload : list byte) : component_result (list component_type) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count)
        (CTFunc {| cf_params := nil; cf_results := nil |}))
  | None => component_err "malformed payload in section 7 (type): invalid count"
  end.

(** Parse section 08 (canon).

    Canonical ABI entries (lift/lower/resource.new/resource.drop/resource.rep).
    We store placeholder canon entries.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/canonicals.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#canonical-abi}}
    *)
Definition parse_canon_payload (payload : list byte) : component_result (list canon) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count)
        (CanonLift 0%N nil 0%N))
  | None => component_err "malformed payload in section 8 (canon): invalid count"
  end.

(** Parse section 0a (component import).

    Component-level imports have a different encoding than core module imports
    (they use an externname + component extern descriptor).  We store
    placeholder [module_import] values.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/imports.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-imports-and-exports}}
    *)
Definition parse_component_import_payload (payload : list byte) : component_result (list import) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count)
        {| imp_module := nil; imp_name := nil;
           imp_desc := MID_func 0%N |})
  | None => component_err "malformed payload in section 0a (import): invalid count"
  end.

(** Parse section 0b (component export).

    Component-level exports also differ from core exports.  We store
    placeholder [module_export] values.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/exports.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-imports-and-exports}}
    *)
Definition parse_component_export_payload (payload : list byte) : component_result (list export) :=
  match parse_payload_count payload with
  | Some (count, _) =>
      component_ok (replicate (N.to_nat count)
        {| modexp_name := nil; modexp_desc := MED_func 0%N |})
  | None => component_err "malformed payload in section 0b (export): invalid count"
  end.

Definition parse_start_payload (payload : list byte) : component_result start :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_module_start) with
  | Some s => component_ok s
  | None => component_err "malformed payload in section 9 (start)"
  end.

(** Helper: update a single field of [parse_state] using a functional record update. *)
Definition update_core_modules (ms : list module) (st : parse_state) : parse_state :=
  mk_parse_state
    (st.(ps_core_modules) ++ ms) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_core_instances (is : list core_instance) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) (st.(ps_core_instances) ++ is) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_core_types (ts : list core_type) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) (st.(ps_core_types) ++ ts)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_components (cs : list component) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    (st.(ps_components) ++ cs) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_instances (is : list instance) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) (st.(ps_instances) ++ is) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_aliases (als : list alias) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) (st.(ps_aliases) ++ als)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_types (ts : list component_type) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    (st.(ps_types) ++ ts) st.(ps_canons) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_canons (cs : list canon) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) (st.(ps_canons) ++ cs) st.(ps_start)
    st.(ps_imports) st.(ps_exports).

Definition update_start (s : start) (st : parse_state) : component_result parse_state :=
  match st.(ps_start) with
  | None =>
      component_ok (mk_parse_state
        st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
        st.(ps_components) st.(ps_instances) st.(ps_aliases)
        st.(ps_types) st.(ps_canons) (Some s)
        st.(ps_imports) st.(ps_exports))
  | Some _ => component_err "duplicate section 09 (start)"
  end.

Definition update_imports (imps : list import) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    (st.(ps_imports) ++ imps) st.(ps_exports).

Definition update_exports (exps : list export) (st : parse_state) : parse_state :=
  mk_parse_state
    st.(ps_core_modules) st.(ps_core_instances) st.(ps_core_types)
    st.(ps_components) st.(ps_instances) st.(ps_aliases)
    st.(ps_types) st.(ps_canons) st.(ps_start)
    st.(ps_imports) (st.(ps_exports) ++ exps).

(** Dispatch a section payload to the appropriate parser and update state.

    Section 04 (nested component) is handled by the caller
    [parse_sections_aux] which has the recursive fuel needed for re-entry.
    *)
Definition parse_section (sid : byte) (payload : list byte) (st : parse_state) : component_result parse_state :=
  (* Section 00 -- custom: skip silently *)
  if byte_eqb sid (#00) then
    component_ok st

  (* Section 01 -- core module *)
  else if byte_eqb sid (#01) then
    match parse_core_module_payload payload with
    | inl ms => component_ok (update_core_modules ms st)
    | inr e => inr e
    end

  (* Section 02 -- core instance *)
  else if byte_eqb sid (#02) then
    match parse_core_instance_payload payload with
    | inl is => component_ok (update_core_instances is st)
    | inr e => inr e
    end

  (* Section 03 -- core type *)
  else if byte_eqb sid (#03) then
    match parse_core_type_payload payload with
    | inl ts => component_ok (update_core_types ts st)
    | inr e => inr e
    end

  (* Section 04 -- nested component: handled in parse_sections_aux *)
  else if byte_eqb sid (#04) then
    component_err "section 04 (component) must be handled by parse_sections_aux"

  (* Section 05 -- instance *)
  else if byte_eqb sid (#05) then
    match parse_instance_payload payload with
    | inl is => component_ok (update_instances is st)
    | inr e => inr e
    end

  (* Section 06 -- alias *)
  else if byte_eqb sid (#06) then
    match parse_alias_payload payload with
    | inl als => component_ok (update_aliases als st)
    | inr e => inr e
    end

  (* Section 07 -- type *)
  else if byte_eqb sid (#07) then
    match parse_type_payload payload with
    | inl ts => component_ok (update_types ts st)
    | inr e => inr e
    end

  (* Section 08 -- canon *)
  else if byte_eqb sid (#08) then
    match parse_canon_payload payload with
    | inl cs => component_ok (update_canons cs st)
    | inr e => inr e
    end

  (* Section 09 -- start *)
  else if byte_eqb sid (#09) then
    match parse_start_payload payload with
    | inl s => update_start s st
    | inr e => inr e
    end

  (* Section 0a -- component import *)
  else if byte_eqb sid sid_import_id then
    match parse_component_import_payload payload with
    | inl imps => component_ok (update_imports imps st)
    | inr e => inr e
    end

  (* Section 0b -- component export *)
  else if byte_eqb sid sid_export_id then
    match parse_component_export_payload payload with
    | inl exps => component_ok (update_exports exps st)
    | inr e => inr e
    end

  (* Section 0c -- value: unsupported *)
  else if byte_eqb sid sid_value_id then
    component_err
      ("unsupported section 0c (" ++ section_id_name sid ++ ") in strict mode")

  else
    component_err
      ("unsupported/unknown section " ++ byte_to_string sid ++ " in strict mode").


(** Iterate over sections in the binary stream, dispatching each to
    [parse_section].  Section 04 (nested component) is handled inline here
    because it requires recursive re-entry into the section loop. *)
Fixpoint parse_sections_aux (fuel : nat) (bs : list byte) (st : parse_state) (idx : nat) : component_result parse_state :=
  match fuel with
   | O =>
       match bs with
       | [] => component_ok st
       | _ => component_err "section processing exceeded remaining bytes"
       end
   | S fuel' =>
      match bs with
      | [] => component_ok st
      | sid :: bs' =>
          match parse_u32leb bs' with
          | None =>
              component_err
                ("section " ++ string_of_nat idx ++ " (" ++ section_id_name sid ++
                 "): malformed u32 LEB128 length")
          | Some (payload_len, after_len) =>
              match split_bytes payload_len after_len with
              | None =>
                  component_err
                    ("section " ++ string_of_nat idx ++ " (" ++ section_id_name sid ++
                     ") has length " ++ string_of_nat (N.to_nat payload_len) ++
                     " exceeding remaining bytes")
              | Some (payload, remainder) =>
                  (* Section 04 -- nested component: parse recursively *)
                  if byte_eqb sid (#04) then
                    match parse_preamble payload with
                    | inl inner_rest =>
                        match parse_sections_aux fuel' inner_rest empty_parse_state 0 with
                        | inl inner_st =>
                            let c := Build_component
                              inner_st.(ps_core_modules)
                              inner_st.(ps_core_instances)
                              inner_st.(ps_core_types)
                              inner_st.(ps_components)
                              inner_st.(ps_instances)
                              inner_st.(ps_aliases)
                              inner_st.(ps_types)
                              inner_st.(ps_canons)
                              inner_st.(ps_start)
                              inner_st.(ps_imports)
                              inner_st.(ps_exports) in
                            let st' := update_components (c :: nil) st in
                            parse_sections_aux fuel' remainder st' (S idx)
                        | inr e => inr ("in nested component: " ++ e)%string
                        end
                    | inr e => inr ("in nested component: " ++ e)%string
                    end
                  else
                    match parse_section sid payload st with
                    | inl st' => parse_sections_aux fuel' remainder st' (S idx)
                    | inr e => inr e
                    end
              end
          end
      end
   end.

Definition parse_sections (bs : list byte) (st : parse_state) (idx : nat) : component_result parse_state :=
  parse_sections_aux (List.length bs) bs st idx.

Definition build_component (st : parse_state) : component :=
  Build_component
    st.(ps_core_modules)
    st.(ps_core_instances)
    st.(ps_core_types)
    st.(ps_components)
    st.(ps_instances)
    st.(ps_aliases)
    st.(ps_types)
    st.(ps_canons)
    st.(ps_start)
    st.(ps_imports)
    st.(ps_exports).

Definition run_parse_component (bs : list byte) : component_result component :=
  match parse_preamble bs with
  | inl rest =>
    match parse_sections rest empty_parse_state 0 with
    | inl st => component_ok (build_component st)
    | inr e => inr e
    end
  | inr e => inr e
  end.

Definition run_parse_component_str (s : String.string) : component_result component :=
  run_parse_component (List.map compcert_byte_of_byte (String.list_byte_of_string s)).
