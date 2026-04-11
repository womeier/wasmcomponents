From Stdlib Require Import String List ZArith.
From Stdlib Require Import Strings.String.
From compcert Require Import Integers.
From Wasm Require Import datatypes binary_format_printer pp.
From wasmcomponents Require Import component_ast component_binary_parser.

Import ListNotations.
Import Stdlib.Strings.String.StringSyntax.
Open Scope N_scope.
Open Scope string_scope.

Definition cbp_preamble : list byte :=
  Byte.repr 0 :: Byte.repr 97 :: Byte.repr 115 :: Byte.repr 109 ::
  Byte.repr 13 :: Byte.repr 0 :: Byte.repr 1 :: Byte.repr 0 :: nil.

Definition cbp_empty_component : component :=
  Build_component nil nil nil nil nil nil nil nil None nil nil.

Definition cbp_empty_module : module :=
  {| mod_types := nil;
     mod_funcs := nil;
     mod_tables := nil;
     mod_mems := nil;
     mod_globals := nil;
     mod_elems := nil;
     mod_datas := nil;
     mod_start := None;
     mod_imports := nil;
     mod_exports := nil
   |}.

Definition cbp_empty_module_component : component :=
  Build_component
    (cbp_empty_module :: nil) nil nil nil nil nil nil nil None nil nil.

Definition mk_section (sid : byte) (payload : list byte) : list byte :=
  sid :: List.map compcert_byte_of_byte (with_length (List.map byte_of_compcert_byte payload)).

Lemma parse_empty_component :
  run_parse_component cbp_preamble = inl cbp_empty_component.
Proof. vm_compute. reflexivity. Qed.

Lemma parse_bad_preamble :
  run_parse_component
    (Byte.repr 0 :: Byte.repr 97 :: Byte.repr 115 :: Byte.repr 109 ::
      Byte.repr 13 :: Byte.repr 0 :: Byte.repr 1 :: Byte.repr 1 :: nil)
    = inr "invalid preamble: expected 00 61 73 6d 0d 00 01 00".
Proof. vm_compute. reflexivity. Qed.

Lemma parse_malformed_section_length :
  run_parse_component (cbp_preamble ++ (Byte.repr 1 :: Byte.repr 128 :: Byte.repr 128 :: Byte.repr 128 :: Byte.repr 128 :: Byte.repr 128 :: nil))
    = inr "section 0 (core_module): malformed u32 LEB128 length".
Proof. vm_compute. reflexivity. Qed.

(** Custom sections (id 0) are now silently skipped. *)
Lemma parse_custom_section_skipped :
  run_parse_component (cbp_preamble ++ mk_section (Byte.repr 0) nil)
    = inl cbp_empty_component.
Proof. vm_compute. reflexivity. Qed.

Definition cbp_core_type_payload : list byte := Byte.repr 0 :: nil.
Definition cbp_core_type_section : list byte := mk_section (Byte.repr 3) cbp_core_type_payload.

Lemma parse_core_type_section :
  run_parse_component (cbp_preamble ++ cbp_core_type_section) = inl cbp_empty_component.
Proof. vm_compute. reflexivity. Qed.

Definition cbp_core_module_section : list byte := mk_section (Byte.repr 1) (List.map compcert_byte_of_byte (binary_of_module cbp_empty_module)).

Definition cbp_start_payload : list byte := List.map compcert_byte_of_byte (binary_of_module_start {| modstart_func := 0 |}).
Definition cbp_start_section : list byte := mk_section (Byte.repr 9) cbp_start_payload.

Lemma parse_duplicate_start_section :
  run_parse_component (cbp_preamble ++ cbp_start_section ++ cbp_start_section)
    = inr "duplicate section 09 (start)".
Proof. vm_compute. reflexivity. Qed.

Definition cbp_malformed_core_module_payload : list byte := Byte.repr 128 :: nil.
Definition cbp_malformed_core_module_section : list byte := mk_section (Byte.repr 1) cbp_malformed_core_module_payload.

Lemma parse_malformed_core_module_payload :
  run_parse_component (cbp_preamble ++ cbp_malformed_core_module_section)
    = inr "malformed payload in section 1 (core_module)".
Proof. vm_compute. reflexivity. Qed.

(** Component import section (id 0x0a) with count=1 produces a placeholder. *)
Definition cbp_import_payload_1 : list byte := Byte.repr 1 :: nil.
Definition cbp_import_section_1 : list byte := mk_section (Byte.repr 10) cbp_import_payload_1.

(** Component export section (id 0x0b) with count=1 produces a placeholder. *)
Definition cbp_export_payload_1 : list byte := Byte.repr 1 :: nil.
Definition cbp_export_section_1 : list byte := mk_section (Byte.repr 11) cbp_export_payload_1.

Lemma parse_component_import_placeholder :
  run_parse_component (cbp_preamble ++ cbp_import_section_1)
    = inl (Build_component nil nil nil nil nil nil nil nil None
             ({| imp_module := nil; imp_name := nil;
                 imp_desc := MID_func 0%N |} :: nil) nil).
Proof. vm_compute. reflexivity. Qed.

Lemma parse_component_export_placeholder :
  run_parse_component (cbp_preamble ++ cbp_export_section_1)
    = inl (Build_component nil nil nil nil nil nil nil nil None nil
             ({| modexp_name := nil; modexp_desc := MED_func 0%N |} :: nil)).
Proof. vm_compute. reflexivity. Qed.

Definition cbp_preamble_string : String.string := String.string_of_list_byte (List.map byte_of_compcert_byte cbp_preamble).

Lemma parse_preamble_string :
  run_parse_component_str cbp_preamble_string = inl cbp_empty_component.
Proof. vm_compute. reflexivity. Qed.

Lemma parse_core_module_section :
  run_parse_component (cbp_preamble ++ cbp_core_module_section) = inl cbp_empty_module_component.
Proof. vm_compute. reflexivity. Qed.
