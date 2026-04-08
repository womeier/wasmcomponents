From Stdlib Require Import Extraction.
From Stdlib Require Import ExtrOcamlBasic.
From Stdlib Require Import ExtrOcamlString.
From Stdlib Require Import ExtrOcamlNatInt.

From wasmcomponents Require Import wit_ast wit_parser.

Set Extraction Output Directory "src/extraction".
Separate Extraction parse_wit.
