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

Definition parse_core_module_payload (payload : list byte) : component_result (list module) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_module) with
  | Some ms => component_ok (ms :: nil)
  | None => component_err "malformed payload in section 1 (core_module)"
  end.

Definition parse_core_type_payload (payload : list byte) : component_result (list core_type) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_vec parse_function_type) with
  | Some ts => component_ok ts
  | None => component_err "malformed payload in section 3 (core_type)"
  end.

Definition parse_start_payload (payload : list byte) : component_result start :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_module_start) with
  | Some s => component_ok s
  | None => component_err "malformed payload in section 9 (start)"
  end.

Definition parse_import_payload (payload : list byte) : component_result (list import) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_vec parse_module_import) with
  | Some imps => component_ok imps
  | None => component_err "malformed payload in section 10 (import)"
  end.

Definition parse_export_payload (payload : list byte) : component_result (list export) :=
  match run (List.map byte_of_compcert_byte payload) (fun n => parse_vec parse_module_export) with
  | Some exps => component_ok exps
  | None => component_err "malformed payload in section 11 (export)"
  end.

Definition parse_section (sid : byte) (payload : list byte) (st : parse_state) : component_result parse_state :=
  if byte_eqb sid (#00) then
    component_err
      ("unsupported section 00 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#01) then
    match parse_core_module_payload payload with
    | inl ms =>
        component_ok
          {| ps_core_modules := st.(ps_core_modules) ++ ms
           ; ps_core_instances := st.(ps_core_instances)
           ; ps_core_types := st.(ps_core_types)
           ; ps_components := st.(ps_components)
           ; ps_instances := st.(ps_instances)
           ; ps_aliases := st.(ps_aliases)
           ; ps_types := st.(ps_types)
           ; ps_canons := st.(ps_canons)
           ; ps_start := st.(ps_start)
           ; ps_imports := st.(ps_imports)
           ; ps_exports := st.(ps_exports)
           |}
    | inr e => inr e
    end
  else if byte_eqb sid (#02) then
    component_err
      ("unsupported section 02 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#03) then
    match parse_core_type_payload payload with
    | inl ts =>
        component_ok
          {| ps_core_modules := st.(ps_core_modules)
           ; ps_core_instances := st.(ps_core_instances)
           ; ps_core_types := st.(ps_core_types) ++ ts
           ; ps_components := st.(ps_components)
           ; ps_instances := st.(ps_instances)
           ; ps_aliases := st.(ps_aliases)
           ; ps_types := st.(ps_types)
           ; ps_canons := st.(ps_canons)
           ; ps_start := st.(ps_start)
           ; ps_imports := st.(ps_imports)
           ; ps_exports := st.(ps_exports)
           |}
    | inr e => inr e
    end
  else if byte_eqb sid (#04) then
    component_err
      ("unsupported section 04 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#05) then
    component_err
      ("unsupported section 05 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#06) then
    component_err
      ("unsupported section 06 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#07) then
    component_err
      ("unsupported section 07 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#08) then
    component_err
      ("unsupported section 08 (" ++ section_id_name sid ++ ") in strict mode")
  else if byte_eqb sid (#09) then
    match parse_start_payload payload with
    | inl s =>
        match st.(ps_start) with
        | None =>
            component_ok
              {| ps_core_modules := st.(ps_core_modules)
               ; ps_core_instances := st.(ps_core_instances)
               ; ps_core_types := st.(ps_core_types)
               ; ps_components := st.(ps_components)
               ; ps_instances := st.(ps_instances)
               ; ps_aliases := st.(ps_aliases)
               ; ps_types := st.(ps_types)
               ; ps_canons := st.(ps_canons)
               ; ps_start := Some s
               ; ps_imports := st.(ps_imports)
               ; ps_exports := st.(ps_exports)
               |}
        | Some _ => component_err "duplicate section 09 (start)"
        end
    | inr e => inr e
    end
   else if byte_eqb sid sid_import_id then
    match parse_import_payload payload with
    | inl imps =>
        component_ok
          {| ps_core_modules := st.(ps_core_modules)
           ; ps_core_instances := st.(ps_core_instances)
           ; ps_core_types := st.(ps_core_types)
           ; ps_components := st.(ps_components)
           ; ps_instances := st.(ps_instances)
           ; ps_aliases := st.(ps_aliases)
           ; ps_types := st.(ps_types)
           ; ps_canons := st.(ps_canons)
           ; ps_start := st.(ps_start)
           ; ps_imports := st.(ps_imports) ++ imps
           ; ps_exports := st.(ps_exports)
           |}
    | inr e => inr e
    end
   else if byte_eqb sid sid_export_id then
    match parse_export_payload payload with
    | inl exps =>
        component_ok
          {| ps_core_modules := st.(ps_core_modules)
           ; ps_core_instances := st.(ps_core_instances)
           ; ps_core_types := st.(ps_core_types)
           ; ps_components := st.(ps_components)
           ; ps_instances := st.(ps_instances)
           ; ps_aliases := st.(ps_aliases)
           ; ps_types := st.(ps_types)
           ; ps_canons := st.(ps_canons)
           ; ps_start := st.(ps_start)
           ; ps_imports := st.(ps_imports)
           ; ps_exports := st.(ps_exports) ++ exps
           |}
    | inr e => inr e
    end
   else if byte_eqb sid sid_value_id then
    component_err
      ("unsupported section 0c (" ++ section_id_name sid ++ ") in strict mode")
  else
    component_err
      ("unsupported/unknown section " ++ byte_to_string sid ++ " in strict mode").


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
