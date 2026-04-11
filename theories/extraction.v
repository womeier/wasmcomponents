From Stdlib Require Import Extraction.
From Stdlib Require Import ExtrOcamlBasic.
From Stdlib Require Import ExtrOcamlString.
From Stdlib Require Import ExtrOcamlNatInt.
From Stdlib Require Import Strings.String.

From wasmcomponents Require Import wit_ast component_ast component_binary_parser wit_parser.

Set Extraction Output Directory "src/extraction".

Separate Extraction parse_wit run_parse_component_str.

