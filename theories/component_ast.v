(** * Component Abstract Syntax

    Defines the core component syntax tree used by the binary parser and later
    lowering/lifting passes.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/types.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#values}}
    *)

From Stdlib Require Import String List BinNat.
Import ListNotations.
From Wasm Require Import datatypes.

(** * Compatibility aliases for unresolved names

    These names are aligned to the section 2.1 sketch in `PLAN.md` and bridge
    directly to available WasmCert-Coq datatypes where possible.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-types}}
    *)
Definition core_type := function_type.
Definition instance := moduleinst.
Definition alias := string.
Definition start := module_start.
Definition import := module_import.
Definition export := module_export.

(** ** Core instance definitions

    Component binary section 02.  A core instance is *defined* by a component,
    not handed to it: either a core module instantiated with arguments, or a
    bag of exports gathered from things already in scope.

    [CID_instantiate midx args] supplies one core instance per *first-level*
    import name.  Core Wasm imports are two-level, so the adapter's
    [(import "rocq" "memory" ...)] resolves by finding ["rocq"] among [args]
    and then ["memory"] among that instance's exports.  That is the mechanism
    this project uses instead of a static link: the CertiRocq module and the
    adapter are two core modules of one component, and the adapter's imports
    are resolved against the CertiRocq instance.  [core_instantiate.v] gives
    it a semantics.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/instances.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#core-instance-definitions}}
    *)
Inductive core_sort :=
  | CSFunc | CSTable | CSMemory | CSGlobal | CSType | CSModule | CSInstance.

(** One [(with "<name>" (instance <i>))] argument.  The MVP admits only
    instances here, so the payload is a core instance index. *)
Record core_instantiate_arg := {
  cia_name     : name;
  cia_instance : N
}.

Record core_inline_export := {
  cie_name : name;
  cie_sort : core_sort;
  cie_idx  : N
}.

Inductive core_instance_def :=
  (** [(core instance (instantiate <moduleidx> <args>))] *)
  | CID_instantiate : N -> list core_instantiate_arg -> core_instance_def
  (** [(core instance (export "n" (func <i>)) ...)] *)
  | CID_inline_exports : list core_inline_export -> core_instance_def.

Definition core_instance := core_instance_def.

(** ** Canonical string encodings

    The component canonical ABI currently uses UTF-8 encoding for textual
    lowering options.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs#L1}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#canonical-abi}}
    *)
Inductive string_encoding :=
  | StringEncodingUtf8.

(** ** Component Value Types

    The value type universe for component functions and aggregate types.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wasmparser/src/readers/component/types.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#types}}
    *)
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

(** ** Component function types

    Function signatures in components, with named parameters and named results.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#functions}}
    *)
Record component_functype := {
  cf_params : list (string * component_valtype);
  cf_results : list (string * component_valtype);
}.

(** ** Component type members

    Named type declarations used in nested component and instance type graphs.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#type-definitions}}
    *)
Inductive component_typedef :=
  | CTD : string -> component_type -> component_typedef

with component_type :=
  | CTFunc : component_functype -> component_type
  | CTComponent : list component_typedef -> component_type
  | CTInstance : list component_typedef -> component_type
  | CTResource : option funcidx -> option funcidx -> component_type.

(** ** Import/export sort

    Sort tags for the component import/export and aliasing operations.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-imports-and-exports}}
    *)
Inductive sort :=
  | SortCoreFunc | SortCoreTable | SortCoreMemory | SortCoreGlobal
  | SortCoreType | SortCoreModule | SortCoreInstance
  | SortFunc | SortValue | SortType | SortComponent | SortInstance.

(** ** Canonical ABI options and operations

    Per-function lowering/lifting options from the canonical ABI and the canonical
    ABI operations that consume them.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#canonical-abi}}
    *)
Inductive canon :=
  | CanonLift : funcidx -> list canonopt -> typeidx -> canon
  | CanonLower : funcidx -> list canonopt -> canon
  | CanonResourceNew : typeidx -> canon
  | CanonResourceDrop : typeidx -> canon
  | CanonResourceRep : typeidx -> canon
with canonopt :=
  | CanonOptMemory : memidx -> canonopt
  | CanonOptRealloc : funcidx -> canonopt
  | CanonOptPostReturn : funcidx -> canonopt
  | CanonOptStringEncoding : string_encoding -> canonopt.

(** ** Component definition

    A complete component, including embedded core modules, nested components,
    core instances, and canonical ABI entries.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#components}}
    *)
Inductive component : Type :=
  | Build_component
      (comp_core_modules : list module)
      (comp_core_instances : list core_instance_def)
      (comp_core_types : list core_type)
      (comp_components : list component)
      (comp_instances : list instance)
      (comp_aliases : list alias)
      (comp_types : list component_type)
      (comp_canons : list canon)
      (comp_start : option start)
      (comp_imports : list import)
      (comp_exports : list export).
