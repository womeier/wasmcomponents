(** * Component Runtime Values

    Runtime value inhabitants of [component_valtype] (see [component_ast.v]).
    These are the values that flow through component function calls and
    traces.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#values}} *)

From Stdlib Require Import ZArith List String.
Import ListNotations.

Open Scope Z_scope.

(** ** Component value type

    A minimal value universe covering the types used by the trace
    examples: integers, booleans, unit, strings, and structural
    combinators.

    Rust: {{https://github.com/bytecodealliance/wasm-tools/blob/cdc92a8f2eb1ef8ec9dbc78fd09f80b96dee282c/crates/wit-component/src/encoding/types.rs}}
    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#values}} *)
Inductive component_value : Type :=
  | CVI32  : Z -> component_value
  | CVI64  : Z -> component_value
  | CVBool : bool -> component_value
  | CVUnit : component_value
  | CVStr  : string -> component_value
  | CVPair : component_value -> component_value -> component_value
  | CVList : list component_value -> component_value.

(** Equality on [component_value] is recovered through Rocq's generic
    [eq] (used by [reflexivity] in the example proofs).  A boolean
    decidable equality can be derived later via [decide equality] or a
    fuel-bounded definition; it is not needed for the trace semantics
    itself or for the two examples in [theories/examples/]. *)
