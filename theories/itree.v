(** * Interaction Trees (minimal, self-contained)

    A minimal inductive definition of interaction trees sufficient for
    modeling component trace semantics.  An [itree R] is a computation
    that can:
    - return a value [Ret r],
    - perform a silent internal step [Tau t], or
    - perform an observable event [Vis e k] and continue with [k] applied
      to the event's response (always a [list component_value] for our
      domain).

    This is the monomorphic specialisation of the ITree of Xia et al.,
    "Interaction Trees", POPL 2020.  We keep it inductive (no coinduction):
    divergence is modelled by running out of fuel during [trace_of]. *)

From Stdlib Require Import List String.
Import ListNotations.
From wasmcomponents Require Import component_values.

(** ** Component event functor

    An event the component can perform during execution.  Currently a
    single constructor: an outgoing call to an imported function. *)
Inductive comp_event : Type :=
  | CECall : string -> list component_value -> comp_event.

(** ** Interaction tree type

    [itree R] is parameterised only by its result type [R]; the event
    functor is fixed to [comp_event].  The type parameter [R] is made
    implicit so that constructors can be written as [Ret r], [Tau t],
    [Vis e k]. *)
Inductive itree (R : Type) : Type :=
  | Ret : R -> itree R
  | Tau : itree R -> itree R
  | Vis : comp_event -> (list component_value -> itree R) -> itree R.

Arguments Ret {R} _.
Arguments Tau {R} _.
Arguments Vis {R} _ _.

(** ** Monadic bind

    Standard ITree [bind] is not structurally recursive on the tree
    because of the function under [Vis].  Rather than carry around a
    fuel parameter (which requires a default value of the result type
    that is not always available), we omit [bind] here: the two example
    components in [theories/examples/] build their behaviour trees
    directly from [Ret], [Tau], and [Vis].  A fuel-bounded or
    well-founded [bind] can be added when needed. *)
