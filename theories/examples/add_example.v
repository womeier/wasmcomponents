(** * Example: Addition of two i32s

    A stateless component with a single export [add : (i32, i32) -> i32]
    returning the sum of its arguments.  Demonstrates the simplest case
    of the trace semantics: the trace contains only the inbound call and
    the final return, with no intermediate [EOutbound] events.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#functions}} *)

From Stdlib Require Import ZArith List String.
Import ListNotations.
From wasmcomponents Require Import component_values itree trace_events trace_semantics.

Open Scope Z_scope.
Open Scope string_scope.

(** ** Component instance

    State is [unit] (the component is stateless). *)
Definition add_inst : component_inst unit :=
  fun _ name args =>
    match name, args with
    | "add", [CVI32 a; CVI32 b] => Ret (OReturn (tt, [CVI32 (a + b)]))
    | _,    _                   => Ret (OTrap "add: unknown function or bad args")
    end.

(** ** Trace property

    For any two i32 values [a] and [b], calling [add(a, b)] yields the
    two-element trace [EInbound "add" [a;b]; EReturn [a+b]] leaving the
    (trivial) state unchanged. *)
Lemma add_trace_correct :
  forall a b,
    call_trace add_inst tt "add" [CVI32 a; CVI32 b] empty_env 10%nat =
      Some ( [EInbound "add" [CVI32 a; CVI32 b]; EReturn [CVI32 (a + b)]]
           , tt
           , OReturn [CVI32 (a + b)] ).
Proof. reflexivity. Qed.

(** ** Roundtrip against a (host) twin

    To show that the trace really captures I/O, we relate [add_inst] to
    a host-side specification function [add_spec]. *)
Definition add_spec (a b : Z) : list component_value := [CVI32 (a + b)].

Lemma add_trace_matches_spec :
  forall a b,
    call_trace add_inst tt "add" [CVI32 a; CVI32 b] empty_env 10%nat =
      Some ( [EInbound "add" [CVI32 a; CVI32 b]; EReturn (add_spec a b)]
           , tt
           , OReturn (add_spec a b) ).
Proof. reflexivity. Qed.

(** ** Print Assumptions check: the proof uses no axioms. *)
Print Assumptions add_trace_correct.
