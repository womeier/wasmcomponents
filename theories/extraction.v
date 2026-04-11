From Stdlib Require Import Extraction.
From Stdlib Require Import ExtrOcamlBasic.
From Stdlib Require Import ExtrOcamlString.
From Stdlib Require Import ExtrOcamlNatInt.
From Stdlib Require Import Strings.String.

From wasmcomponents Require Import wit_ast component_ast component_binary_parser wit_parser.

Set Extraction Output Directory "src/extraction".

Definition parse_component_ok (s : string) : bool :=
  match run_parse_component_str s with
  | inl _ => true
  | inr _ => false
  end.

Definition parse_component_error (s : string) : option string :=
  match run_parse_component_str s with
  | inl _ => None
  | inr e => Some e
  end.

Separate Extraction parse_wit parse_component_ok parse_component_error.
