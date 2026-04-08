# Rocq Formalization of the Wasm Component Model

## Overview

This project formalizes the WebAssembly Component Model in Rocq, building on
WasmCert-Coq for core Wasm definitions. The goal is to have a verified
implementation that can:

1. Parse and print WIT files
2. Represent the component AST (with parser/printer tested against wasm-tools)
3. Transform a core Wasm module into a component given a WIT interface
4. Demonstrate correctness with simple and complex examples

## Documentation Standards

Every definition in the formalization must include a comment with:
1. A concise description of what the definition represents
2. A link to the corresponding Rust implementation in `wasm-tools` (when applicable)
3. A link to the relevant section in the component model spec (when applicable)

Example:
```coq
(** Component value types.
    Extends core Wasm types with higher-level types for the component model.

    Rust: https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/types.rs
    Spec: https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#type-definitions *)
Inductive component_valtype := ...
```

Submodule versions used:
- `wasm-tools`: cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c
- `component-model`: 9a183e56f5c6cc3217895adf54cc4f62de4fa5c9
- `WASI`: 5b0aa5140c4021071a4acdc35ec2b56a2fb45854
- `wasmcert-coq`: 6acc7be59d1ac23881e4d153894616d3a41affcf

## Phase 1: WIT File Support

### 1.1 WIT AST Definition

Define the abstract syntax for WIT in `theories/wit/wit_ast.v`:

```
(* WIT primitive types *)
Inductive wit_primitive :=
  | WitU8 | WitU16 | WitU32 | WitU64
  | WitS8 | WitS16 | WitS32 | WitS64
  | WitF32 | WitF64
  | WitChar | WitString | WitBool.

(* WIT type definitions *)
Inductive wit_type :=
  | WitPrim : wit_primitive -> wit_type
  | WitList : wit_type -> wit_type
  | WitOption : wit_type -> wit_type
  | WitResult : option wit_type -> option wit_type -> wit_type
  | WitTuple : list wit_type -> wit_type
  | WitRecord : list (string * wit_type) -> wit_type
  | WitVariant : list (string * option wit_type) -> wit_type
  | WitEnum : list string -> wit_type
  | WitFlags : list string -> wit_type
  | WitResource : string -> wit_type
  | WitOwn : string -> wit_type
  | WitBorrow : string -> wit_type
  | WitStream : option wit_type -> wit_type
  | WitFuture : option wit_type -> wit_type
  | WitNamed : string -> wit_type.

(* Function signature *)
Record wit_func := {
  func_name : string;
  func_params : list (string * wit_type);
  func_results : list wit_type;  (* can be named or anonymous *)
}.

(* Interface definition *)
Record wit_interface := {
  iface_name : string;
  iface_types : list (string * wit_type);
  iface_funcs : list wit_func;
  iface_resources : list string;
}.

(* World definition *)
Record wit_world := {
  world_name : string;
  world_imports : list (string * wit_interface);
  world_exports : list (string * wit_interface);
}.

(* Package definition *)
Record wit_package := {
  pkg_namespace : string;
  pkg_name : string;
  pkg_version : option string;
  pkg_interfaces : list wit_interface;
  pkg_worlds : list wit_world;
}.
```

### 1.2 WIT Parser (OCaml extraction)

Create `theories/wit/wit_parser.v` with:
- Lexer for WIT tokens (keywords, identifiers, punctuation)
- Recursive descent parser for WIT grammar
- Parser combinators following WasmCert style

The parser should be extracted to OCaml via `extraction/extract.v`, with:
- Scaffolding (dune build files, entry point) to build a standalone OCaml binary
- A Python script to run the extracted parser on the `wasm-tools` WIT parser test suite (`crates/wit-parser/tests/`) and compare results

### 1.3 WIT Printer

Create `theories/wit/wit_printer.v`:
- Pretty printer for WIT AST back to text
- Roundtrip property: parse . print . parse = parse

### 1.4 Test Suite Integration

Use the test cases from `wasm-tools/crates/wit-parser/tests/`:
- Parse all `.wit` files in the test suite
- Verify roundtrip for valid files
- Verify expected errors for invalid files

## Phase 2: Component AST

### 2.1 Component Type Definitions

Define in `theories/component/component_ast.v`, following the binary format spec:

```
(* Component value types - extends core Wasm types *)
Inductive component_valtype :=
  | CVBool
  | CVS8 | CVU8 | CVS16 | CVU16 | CVS32 | CVU32 | CVS64 | CVU64
  | CVF32 | CVF64
  | CVChar | CVString
  | CVList : component_valtype -> component_valtype
  | CVRecord : list (string * component_valtype) -> component_valtype
  | CVTuple : list component_valtype -> component_valtype
  | CVVariant : list (string * option component_valtype) -> component_valtype
  | CVEnum : list string -> component_valtype
  | CVOption : component_valtype -> component_valtype
  | CVResult : option component_valtype -> option component_valtype -> component_valtype
  | CVFlags : list string -> component_valtype
  | CVOwn : typeidx -> component_valtype
  | CVBorrow : typeidx -> component_valtype.

(* Component function type *)
Record component_functype := {
  cf_params : list (string * component_valtype);
  cf_results : list (string * component_valtype);
}.

(* Component type *)
Inductive component_type :=
  | CTFunc : component_functype -> component_type
  | CTComponent : list component_typedef -> component_type
  | CTInstance : list component_typedef -> component_type
  | CTResource : option funcidx -> option funcidx -> component_type.

(* Sort indices for imports/exports *)
Inductive sort :=
  | SortCoreFunc | SortCoreTable | SortCoreMemory | SortCoreGlobal
  | SortCoreType | SortCoreModule | SortCoreInstance
  | SortFunc | SortValue | SortType | SortComponent | SortInstance.

(* Canonical ABI definitions *)
Inductive canon :=
  | CanonLift : funcidx -> list canonopt -> typeidx -> canon
  | CanonLower : funcidx -> list canonopt -> canon
  | CanonResourceNew : typeidx -> canon
  | CanonResourceDrop : typeidx -> canon
  | CanonResourceRep : typeidx -> canon.

Inductive canonopt :=
  | CanonOptMemory : memidx -> canonopt
  | CanonOptRealloc : funcidx -> canonopt
  | CanonOptPostReturn : funcidx -> canonopt
  | CanonOptStringEncoding : string_encoding -> canonopt.

(* Component definition *)
Record component := {
  comp_core_modules : list module;           (* embedded core modules *)
  comp_core_instances : list core_instance;
  comp_core_types : list core_type;
  comp_components : list component;          (* nested components *)
  comp_instances : list instance;
  comp_aliases : list alias;
  comp_types : list component_type;
  comp_canons : list canon;
  comp_start : option start;
  comp_imports : list import;
  comp_exports : list export;
}.
```

### 2.2 Component Binary Parser

Create `theories/component/component_binary_parser.v`:
- Extend WasmCert binary parser for component sections
- Handle component preamble (magic, version 0x0d, layer 0x01)
- Parse all 12 section types

### 2.3 Component Binary Printer

Create `theories/component/component_binary_printer.v`:
- Serialize component AST to binary format
- Roundtrip property with parser

### 2.4 Test Suite

Test against `wasm-tools/crates/wit-component/tests/components/`:
- Parse all `.wasm` component files
- Verify roundtrip for binary format
- Compare with wasm-tools output

## Phase 3: Core Module to Component Transformation

### 3.1 Canonical ABI

Define the Canonical ABI in `theories/component/canonical_abi.v`:

```
(* Flatten a component type to core Wasm types *)
Fixpoint flatten (t : component_valtype) : list value_type := ...

(* Lift: wrap a core function as a component function *)
Definition lift (core_func : funcidx) (opts : list canonopt)
                (comp_type : component_functype) : canon := ...

(* Lower: wrap a component function as a core function *)
Definition lower (comp_func : funcidx) (opts : list canonopt) : canon := ...
```

Key operations:
- `flatten`: convert component types to core types
- `lift`: adapt core function exports to component interface
- `lower`: adapt component imports to core function calls
- Memory management with `realloc` for dynamic types

### 3.2 WIT to Component Type Translation

Create `theories/component/wit_to_component.v`:

```
(* Translate WIT types to component types *)
Definition wit_type_to_component (wt : wit_type) : component_valtype := ...

(* Translate WIT interface to component instance type *)
Definition wit_interface_to_instance (iface : wit_interface) : component_type := ...

(* Translate WIT world to component type *)
Definition wit_world_to_component (world : wit_world) : component_type := ...
```

### 3.3 Module Wrapping

Create `theories/component/module_to_component.v`:

```
(* Given a core module and a WIT world, produce a component *)
Definition wrap_module (m : module) (world : wit_world) : option component := ...
```

This function:
1. Validates that module exports match WIT world exports
2. Validates that module imports match WIT world imports
3. Generates `canon lift` for each export
4. Generates `canon lower` for each import
5. Wraps in component structure with correct type section

### 3.4 Correctness Theorem

```
Theorem wrap_module_correct:
  forall m world c,
    wrap_module m world = Some c ->
    component_valid c /\
    component_exports_match c world.(world_exports) /\
    component_imports_match c world.(world_imports).
```

## Phase 4: Simple Example - Add Function

### 4.1 WIT Interface

```wit
package example:adder@0.1.0;

interface adder {
    add: func(a: s32, b: s32) -> s32;
}

world adder-world {
    export adder;
}
```

### 4.2 Core Module

A simple core Wasm module that exports an `add` function:

```
(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add))
```

### 4.3 Expected Component

The wrapped component should:
1. Embed the core module
2. Instantiate it
3. Lift the `add` function with appropriate canonical options
4. Export the lifted function under the interface name

### 4.4 Verification

```
Definition add_wit : wit_world := ...
Definition add_module : module := ...
Definition add_component : component := wrap_module add_module add_wit.

Theorem add_example_correct:
  component_valid add_component.
```

## Phase 5: Complex Example - WASI Insecure Random

### 5.1 WIT Interface (from WASI)

```wit
package wasi:random@0.2.11;

interface insecure {
    get-insecure-random-bytes: func(len: u64) -> list<u8>;
    get-insecure-random-u64: func() -> u64;
}

world random-consumer {
    import wasi:random/insecure@0.2.11;
    export run: func() -> u64;
}
```

### 5.2 Core Module

Module that imports random and uses it:

```
(module
  ;; Import the lowered random function
  (import "wasi:random/insecure@0.2.11" "get-insecure-random-u64"
          (func $random_u64 (result i64)))

  ;; Export a function that calls random
  (func (export "run") (result i64)
    call $random_u64))
```

### 5.3 Component Structure

The component needs:
1. Import declaration for `wasi:random/insecure`
2. `canon lower` to adapt the imported function
3. Embedded core module with the import satisfied
4. `canon lift` for the exported `run` function

### 5.4 Verification

```
Theorem random_example_correct:
  forall impl,
    implements_insecure_random impl ->
    let c := wrap_module random_module random_wit in
    component_valid c /\
    forall result,
      component_call c "run" [] = Some result ->
      result = impl_call impl "get-insecure-random-u64" [].
```

## File Structure

```
theories/
├── wit/
│   ├── wit_ast.v           # WIT abstract syntax
│   ├── wit_parser.v        # WIT text parser
│   ├── wit_printer.v       # WIT pretty printer
│   └── wit_tests.v         # Parser/printer tests
├── component/
│   ├── component_ast.v     # Component AST
│   ├── component_binary_parser.v
│   ├── component_binary_printer.v
│   ├── canonical_abi.v     # Flatten, lift, lower
│   ├── wit_to_component.v  # WIT -> component types
│   ├── module_to_component.v  # Wrapping transformation
│   └── component_tests.v
├── examples/
│   ├── add_example.v       # Simple add function
│   └── random_example.v    # WASI insecure random
└── extraction/
    └── extract.v           # OCaml extraction for tooling
```

## Dependencies on WasmCert-Coq

Reuse from WasmCert:
- `datatypes.v`: Core Wasm types (`value_type`, `functype`, `module`, etc.)
- `binary_format_parser.v`: Binary parsing infrastructure
- `binary_format_printer.v`: Binary printing infrastructure
- `leb128.v`: LEB128 encoding/decoding
- `bytes.v`: Byte manipulation utilities

## Milestones

1. **M1**: WIT AST and parser (2 weeks)
2. **M2**: Component AST and binary parser (3 weeks)
3. **M3**: Canonical ABI and module wrapping (3 weeks)
4. **M4**: Add example verified (1 week)
5. **M5**: Random example verified (2 weeks)

## Open Questions

1. How much of the component validation should be formalized?
2. Should we support component linking/composition?
3. What level of detail for the Canonical ABI memory layout?
4. Should resources be fully modeled or deferred?
