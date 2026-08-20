(** * Example: Turnstile State Machine

    A simple stateful component modelling a turnstile.  States are
    [Locked] and [Unlocked].  Exports:
    - [coin : () -> ()] : insert a coin; transitions [Locked] to
      [Unlocked] (and is a no-op when already [Unlocked]).
    - [push : () -> bool] : push the turnstile; returns [true] and
      transitions to [Locked] iff the state is [Unlocked], otherwise
      returns [false] and stays [Locked].

    Demonstrates stateful traces where the same call produces different
    outcomes depending on the current state. *)

From Stdlib Require Import ZArith List String.
Import ListNotations.
From wasmcomponents Require Import component_values itree trace_events trace_semantics.

Open Scope Z_scope.
Open Scope string_scope.

(** ** State *)
Inductive turnstile_state : Type :=
  | Locked
  | Unlocked.

(** ** Component instance *)
Definition turnstile_inst : component_inst turnstile_state :=
  fun s name args =>
    match name, args with
    | "coin", [] =>
        Ret (OReturn (Unlocked, [CVUnit]))
    | "push", [] =>
        match s with
        | Locked   => Ret (OReturn (Locked,   [CVBool false]))
        | Unlocked => Ret (OReturn (Locked,   [CVBool true]))
        end
    | _, _ =>
        Ret (OTrap "turnstile: unknown function or bad args")
    end.

(** ** Single-call trace properties *)

(** [coin] from any state leaves the turnstile [Unlocked] and returns
    [unit]. *)
Lemma coin_trace_correct :
  forall s,
    call_trace turnstile_inst s "coin" [] empty_env 10%nat =
      Some ( [EInbound "coin" []; EReturn [CVUnit]]
           , Unlocked
           , OReturn [CVUnit] ).
Proof. destruct s; reflexivity. Qed.

(** [push] from [Locked] keeps the state [Locked] and returns [false]. *)
Lemma push_from_locked :
  call_trace turnstile_inst Locked "push" [] empty_env 10%nat =
    Some ( [EInbound "push" []; EReturn [CVBool false]]
         , Locked
         , OReturn [CVBool false] ).
Proof. reflexivity. Qed.

(** [push] from [Unlocked] transitions to [Locked] and returns [true]. *)
Lemma push_from_unlocked :
  call_trace turnstile_inst Unlocked "push" [] empty_env 10%nat =
    Some ( [EInbound "push" []; EReturn [CVBool true]]
         , Locked
         , OReturn [CVBool true] ).
Proof. reflexivity. Qed.

(** ** Sequence property: coin then push always opens

    The canonical turnstile invariant: a [coin] leaves the turnstile
    [Unlocked] regardless of starting state, and from [Unlocked] a
    [push] always returns [true] and ends in [Locked].  Stated as a
    conjunction over the two single-call traces. *)
Lemma coin_then_push_true :
  forall (s : turnstile_state),
    call_trace turnstile_inst s "coin" [] empty_env 10%nat =
      Some ( [EInbound "coin" []; EReturn [CVUnit]]
           , Unlocked
           , OReturn [CVUnit] ) /\
    call_trace turnstile_inst Unlocked "push" [] empty_env 10%nat =
      Some ( [EInbound "push" []; EReturn [CVBool true]]
           , Locked
           , OReturn [CVBool true] ).
Proof. destruct s; split; reflexivity. Qed.

(** ** Print Assumptions check: the proofs use no axioms. *)
Print Assumptions coin_trace_correct.
Print Assumptions push_from_unlocked.
Print Assumptions coin_then_push_true.
