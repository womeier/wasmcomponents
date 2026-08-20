(** * Trace Events and Outcomes

    Defines the observable events of a component execution trace and the
    outcome type distinguishing normal returns from traps.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-imports-and-exports}} *)

From Stdlib Require Import List String.
Import ListNotations.
From wasmcomponents Require Import component_values.

Open Scope string_scope.

(** ** Trace events

    An [event] is a single observable interaction in a component's
    trace.
    - [EInbound name args] records the host calling export [name] with
      [args].
    - [EOutbound name args] records the component calling import [name]
      with [args].
    - [EReturn results] records the component returning from the current
      export call with [results].
    - [ETrap msg] records the component trapping with message [msg]. *)
Inductive event : Type :=
  | EInbound  : string -> list component_value -> event
  | EOutbound : string -> list component_value -> event
  | EReturn   : list component_value -> event
  | ETrap     : string -> event.

Definition trace := list event.

(** ** Execution outcome

    [outcome A] is either a normal return carrying a value of type [A],
    or a trap carrying a message. *)
Inductive outcome (A : Type) : Type :=
  | OReturn : A -> outcome A
  | OTrap   : string -> outcome A.

Arguments OReturn {A} a.
Arguments OTrap   {A} msg.
