(** * Component Trace Semantics

    Extracts a trace of events from a component's ITree behaviour, and
    provides the [call_trace] wrapper that records the inbound call and
    final return/trap as observable events.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#component-imports-and-exports}} *)

From Stdlib Require Import List String.
Import ListNotations.
From wasmcomponents Require Import component_values itree trace_events.

Open Scope string_scope.

(** ** Environment for resolving outbound calls

    An [env] is a partial function from import name + args to a
    response.  Used by [trace_of] to step through [Vis] nodes. *)
Definition env : Type := string -> list component_value -> option (list component_value).

Definition empty_env : env := fun _ _ => None.

(** ** Trace extraction

    [trace_of env fuel t] extracts the maximal finite trace of [t].
    Returns [Some (events, outcome)] if [t] terminates within [fuel]
    steps, [None] otherwise (divergence or out of fuel).

    [Tau] steps are silent (not recorded).  [Vis (CECall name args) k]
    records an [EOutbound] event and continues with [k] applied to the
    environment's response; if the environment has no response for the
    call, extraction is stuck (returns [None]). *)
Fixpoint trace_of {R} (e : env) (fuel : nat) (t : itree (outcome R))
  : option (list event * outcome R) :=
  match fuel with
  | O => None
  | S fuel' =>
      match t with
      | Ret o => Some ([], o)
      | Tau t' => trace_of e fuel' t'
      | Vis (CECall name args) k =>
          match e name args with
          | Some resp =>
              match trace_of e fuel' (k resp) with
              | Some (events, o) => Some (EOutbound name args :: events, o)
              | None => None
              end
          | None => None
          end
      end
  end.

(** ** Component instance type

    A component instance is a transition function: given the current
    state [s], an export name, and arguments, produce an [itree] that
    returns the new state and a result list (or traps). *)
Definition component_inst (S : Type) : Type :=
  S -> string -> list component_value -> itree (outcome (S * list component_value)).

(** ** Full call trace

    [call_trace inst s name args env fuel] runs a single export call
    and wraps the result with [EInbound] (the call) and [EReturn] or
    [ETrap] (the outcome).  Returns [None] if the component diverged
    within [fuel] steps. *)
Definition call_trace {S} (inst : component_inst S) (s : S)
  (name : string) (args : list component_value)
  (e : env) (fuel : nat)
  : option (trace * S * outcome (list component_value)) :=
  match trace_of e fuel (inst s name args) with
  | Some (events, OReturn (s', result)) =>
      Some (EInbound name args :: events ++ [EReturn result], s', OReturn result)
  | Some (events, OTrap msg) =>
      Some (EInbound name args :: events ++ [ETrap msg], s, OTrap msg)
  | None => None
  end.

(** ** Trace equality is recovered through Rocq's generic [eq] (used
    by [reflexivity] in the example proofs).  A boolean decidable
    equality on [trace] can be added later once a boolean equality on
    [component_value] is derived; it is not needed for the trace
    extraction itself or for the two examples in [theories/examples/]. *)
