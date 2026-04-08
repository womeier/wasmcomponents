(** * WIT Abstract Syntax Tree

    Defines the abstract syntax for WIT (WebAssembly Interface Types),
    the interface definition language for the WebAssembly Component Model.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md}} *)

From Stdlib Require Import String List.
Import ListNotations.

(** ** Primitive Types

    The built-in scalar types in WIT.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs#L1}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#primitive-types}} *)
Inductive wit_primitive :=
  | WitU8 | WitU16 | WitU32 | WitU64
  | WitS8 | WitS16 | WitS32 | WitS64
  | WitF32 | WitF64
  | WitChar | WitString | WitBool.

(** ** WIT Types

    The full set of types expressible in WIT, including composite and
    reference types.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#wit-types}} *)
Inductive wit_type :=
  (** Primitive scalar type *)
  | WitPrim    : wit_primitive -> wit_type
  (** Homogeneous sequence of elements *)
  | WitList    : wit_type -> wit_type
  (** Optional value *)
  | WitOption  : wit_type -> wit_type
  (** Success/error result; [None] means the case carries no payload *)
  | WitResult  : option wit_type -> option wit_type -> wit_type
  (** Fixed-length, heterogeneous tuple *)
  | WitTuple   : list wit_type -> wit_type
  (** Named product type (struct) *)
  | WitRecord  : list (string * wit_type) -> wit_type
  (** Tagged union with optional payloads *)
  | WitVariant : list (string * option wit_type) -> wit_type
  (** Closed set of named constants (no payload) *)
  | WitEnum    : list string -> wit_type
  (** Bit-flag set *)
  | WitFlags   : list string -> wit_type
  (** Opaque resource handle – owns the resource *)
  | WitResource : string -> wit_type
  (** Owned handle to a named resource type *)
  | WitOwn     : string -> wit_type
  (** Borrowed handle to a named resource type *)
  | WitBorrow  : string -> wit_type
  (** Async stream of zero or one element types *)
  | WitStream  : option wit_type -> wit_type
  (** Async single-value channel *)
  | WitFuture  : option wit_type -> wit_type
  (** Reference to a named type alias *)
  | WitNamed   : string -> wit_type.

(** ** Function Signature

    A named function with typed parameters and results.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#functions}} *)
Record wit_func := {
  func_name    : string;
  func_params  : list (string * wit_type);
  (** Results may be anonymous (single unnamed return) or named *)
  func_results : list wit_type;
}.

(** ** Interface Definition

    A named collection of type aliases, functions, and resources.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#interfaces}} *)
Record wit_interface := {
  iface_name      : string;
  iface_types     : list (string * wit_type);
  iface_funcs     : list wit_func;
  iface_resources : list string;
}.

(** ** World Definition

    A world describes a component's imports and exports in terms of
    named interfaces.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#worlds}} *)
Record wit_world := {
  world_name    : string;
  world_imports : list (string * wit_interface);
  world_exports : list (string * wit_interface);
}.

(** ** Package Definition

    The top-level unit of a WIT document.  A package groups related
    interfaces and worlds under a namespaced name with an optional
    semantic version.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-parser/src/ast.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/WIT.md#packages}} *)
Record wit_package := {
  pkg_namespace  : string;
  pkg_name       : string;
  pkg_version    : option string;
  pkg_interfaces : list wit_interface;
  pkg_worlds     : list wit_world;
}.
