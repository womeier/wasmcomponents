(** * Stepping toolkit for hand-written Wasm

    Verifying an adapter means stepping through it in WasmCert's
    [reduce_trans].  No program logic is needed for code this small, but the
    bookkeeping of [r_label] contexts is tedious enough that it has to be
    factored out, and the factoring already exists: CertiRocq's Wasm backend
    proofs use exactly this toolkit.

    The lemmas and tactics below are adapted from
    {{https://github.com/womeier/CertiRocq}} rev 6d34b50b,
    [theories/CodegenWasm/LambdaANF_to_Wasm_utils.v:51-130] and [:1708-1750].
    They depend on nothing outside WasmCert, which is why they can be
    duplicated here; once this repository depends on CertiRocq this file should
    be deleted in favour of the original.

    Usage, as in the backend's proofs:
    - [dostep_nary n] takes one reduction step of an instruction with [n]
      leading constant arguments, leaving the remaining instructions alone;
    - [dostep'] / [dostep_nary' n] are the variants for a goal whose
      right-hand side is a single instruction list;
    - [reduce_trans_label0] / [reduce_trans_label1] get under the [AI_label]
      that [BI_block] and [BI_loop] introduce — the loop bodies in
      [echo_adapter.v] need [label1], since [br 1] exits the enclosing block. *)

From Wasm Require Import datatypes operations memory opsem host properties.

From Stdlib Require Import BinNat List Lia
  Relations.Relations Relations.Relation_Operators.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.

(** ** Splitting an instruction list

    Rewrites [x :: l] into [[x] ++ l] so that the [r_label] lemmas below
    apply. *)
Ltac separate_instr :=
  cbn;
  repeat match goal with
  |- context C [?x :: ?l] =>
     lazymatch l with [::] => fail | _ => rewrite -(cat1s x l) end
  end.

Section Steps.

Context `{ho : host}.

(** A reduction may be prefixed by constants. *)
Lemma r_eliml : forall hs s f es hs' s' f' es' lconst,
  const_list lconst ->
  reduce hs s f es hs' s' f' es' ->
  reduce hs s f (lconst ++ es) hs' s' f' (lconst ++ es').
Proof.
  move => hs s f es hs' s' f' es' lconst HConst H.
  apply const_es_exists in HConst. destruct HConst as [vs ?].
  eapply r_label with (lh:=LH_base vs []). eassumption.
  - cbn. rewrite cats0. congruence.
  - cbn. rewrite cats0. congruence.
Qed.

(** A reduction may be followed by further instructions. *)
Lemma r_elimr : forall hs s f es hs' s' f' es' les,
  reduce hs s f es hs' s' f' es' ->
  reduce hs s f (es ++ les) hs' s' f' (es' ++ les).
Proof.
  move => hs s f es hs' s' f' es' les H.
  eapply r_label with (lh:=LH_base [] les); eauto.
Qed.

(** Stepping under a label, which is how the bodies of [BI_block] and
    [BI_loop] are reached. *)
Lemma reduce_trans_label : forall instr instr' hs hs' sr sr' fr fr' i (lh : lholed i),
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, fr, lfill lh instr) (hs', sr', fr', lfill lh instr').
Proof.
  intros.
  apply clos_rt_rt1n in H.
  remember (hs, sr, fr, instr) as x. remember (hs', sr', fr', instr') as x'.
  generalize dependent hs. generalize dependent hs'.
  revert fr fr' sr sr' instr instr'.
  induction H; intros; subst.
  - inversion Heqx. apply rt_refl.
  - destruct y as [[[? ?] ?] ?].
    eapply rt_trans with (y := (?[hs], ?[sr], ?[f'], ?[instr])).
    eapply rt_step. eapply r_label with (k:=i) (lh:=lh); eauto.
    now apply IHclos_refl_trans_1n.
Qed.

Lemma reduce_trans_label0 : forall instr instr' hs hs' sr sr' fr fr',
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, fr, [:: AI_label 0 [::] instr])
               (hs', sr', fr', [:: AI_label 0 [::] instr']).
Proof.
  intros.
  remember (LH_rec [] 0 [] (LH_base [] []) []) as lh.
  have H' := reduce_trans_label instr instr' _ _ _ _ _ _ _ lh. subst lh. cbn in H'.
  do 2! rewrite cats0 in H'. eapply rt_trans. eapply H'; auto. eassumption.
  apply rt_refl.
Qed.

Lemma reduce_trans_label1 : forall instr instr' hs hs' sr sr' fr fr',
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, fr, [:: AI_label 1 [::] instr])
               (hs', sr', fr', [:: AI_label 1 [::] instr']).
Proof.
  intros.
  remember (LH_rec [] 1 [] (LH_base [] []) []) as lh.
  have H' := reduce_trans_label instr instr' _ _ _ _ _ _ _ lh. subst lh. cbn in H'.
  do 2! rewrite cats0 in H'. eapply rt_trans. eapply H'; auto. eassumption.
  apply rt_refl.
Qed.

(** ** Functoriality of [reduce_trans]

    Everything below is an instance of one observation: a context that turns
    single steps into single steps turns transitive closures into transitive
    closures.  Stating it once avoids re-running the [clos_refl_trans_1n]
    induction for every context. *)
Lemma reduce_trans_map : forall g,
  (forall x y, reduce_tuple x y -> reduce_tuple (g x) (g y)) ->
  forall x y, reduce_trans x y -> reduce_trans (g x) (g y).
Proof.
  intros g Hg x y H. induction H.
  - apply rt_step. now apply Hg.
  - apply rt_refl.
  - eapply rt_trans; eassumption.
Qed.

(** Stepping inside a call frame.  [lfill] does not reach through [AI_frame],
    so this is not an instance of [reduce_trans_label]; note that the enclosing
    frame [f0] is untouched while the inner one may change (a [local.set] in
    the callee is a change of frame). *)
Lemma reduce_trans_frame : forall n f0 instr instr' hs hs' sr sr' fr fr',
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, f0, [:: AI_frame n fr instr])
               (hs', sr', f0, [:: AI_frame n fr' instr']).
Proof.
  intros n f0 instr instr' hs hs' sr sr' fr fr' H.
  assert (Hg : forall x y, reduce_tuple x y ->
    reduce_tuple
      (let '(hs0, sr0, fr0, es0) := x in (hs0, sr0, f0, [:: AI_frame n fr0 es0]))
      (let '(hs0, sr0, fr0, es0) := y in (hs0, sr0, f0, [:: AI_frame n fr0 es0]))).
  { intros [[[??]?]?] [[[??]?]?] Hred. cbn in *. now apply r_frame. }
  have H' := reduce_trans_map _ Hg (hs, sr, fr, instr) (hs', sr', fr', instr') H.
  cbn in H'. exact H'.
Qed.

(** The transitive version of [r_elimr]: a run of steps may be followed by
    further instructions. *)
Lemma reduce_trans_elimr : forall les instr instr' hs hs' sr sr' fr fr',
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, fr, instr ++ les) (hs', sr', fr', instr' ++ les).
Proof.
  intros les instr instr' hs hs' sr sr' fr fr' H.
  have H' := reduce_trans_label instr instr' _ _ _ _ _ _ 0 (LH_base [::] les) H.
  cbn in H'. exact H'.
Qed.

(** [reduce_trans_label0] and [reduce_trans_label1] require the label's
    continuation to be empty, which is what [BI_block] produces.  [BI_loop]
    produces a label whose continuation is the loop itself, so stepping inside
    a loop body needs the version that keeps an arbitrary continuation. *)
Lemma reduce_trans_label_cont : forall n cont instr instr' hs hs' sr sr' fr fr',
  reduce_trans (hs, sr, fr, instr) (hs', sr', fr', instr') ->
  reduce_trans (hs, sr, fr, [:: AI_label n cont instr])
               (hs', sr', fr', [:: AI_label n cont instr']).
Proof.
  intros.
  remember (LH_rec [] n cont (LH_base [] []) []) as lh.
  have H' := reduce_trans_label instr instr' _ _ _ _ _ _ _ lh. subst lh. cbn in H'.
  do 2! rewrite cats0 in H'. eapply rt_trans. eapply H'; auto. eassumption.
  apply rt_refl.
Qed.

(** ** Leaving and continuing a [block]/[loop] pair

    Every loop in the adapter has the shape

      block (loop ... br_if 1 ... br 0)

    so there are exactly two ways out of the body, and both are a single
    [rs_br] step whose only content is the label context it names.  Factoring
    them out here is what keeps the loop proofs in [echo_correct.v] about the
    data rather than about [lfill]. *)

(** [br 1] leaves the enclosing block: the whole construct disappears. *)
Lemma br_block_loop : forall hs sr fr tb es rest,
  reduce_trans
    (hs, sr, fr,
     [:: AI_label 0 [::]
           [:: AI_label 0 [:: AI_basic (BI_loop tb es)]
                 ([:: AI_basic (BI_br 1%N)] ++ rest) ]])
    (hs, sr, fr, [::]).
Proof.
  intros. apply rt_step. apply r_simple.
  eapply rs_br with (vs := [::]) (i := 1)
    (lh := LH_rec [::] 0 [:: AI_basic (BI_loop tb es)] (LH_base [::] rest) [::]);
    try reflexivity.
Qed.

(** [br 0] re-enters the loop: the body is replaced by the loop instruction. *)
Lemma br_loop_continue : forall hs sr fr tb es rest,
  reduce_trans
    (hs, sr, fr,
     [:: AI_label 0 [:: AI_basic (BI_loop tb es)]
           ([:: AI_basic (BI_br 0%N)] ++ rest) ])
    (hs, sr, fr, [:: AI_basic (BI_loop tb es)]).
Proof.
  intros. apply rt_step. apply r_simple.
  eapply rs_br with (vs := [::]) (i := 0) (lh := LH_base [::] rest);
    try reflexivity.
Qed.

End Steps.

(** ** Isolating an instruction with its leading arguments

    e.g. with [n = 2] for an [i32.add]:
    [[const 1, const 2, add, rest]] becomes [[const 1, const 2, add]]. *)
Ltac elimr_nary_instr n :=
  let H := fresh "H" in
  match n with
  | 0 => lazymatch goal with
         | |- reduce _ _ _ ([:: ?instr])        _ _ _ _ => idtac
         | |- reduce _ _ _ ([:: ?instr] ++ ?l3) _ _ _ _ => apply r_elimr
         end
  | 1 => lazymatch goal with
         | |- reduce _ _ _ ([::$VN ?c1] ++ [:: ?instr])        _ _ _ _ => idtac
         | |- reduce _ _ _ ([::$VN ?c1] ++ [:: ?instr] ++ ?l3) _ _ _ _ =>
            assert ([::$VN c1] ++ [:: instr] ++ l3 =
                    [:: $VN c1; instr] ++ l3) as H by reflexivity; rewrite H;
                                                       apply r_elimr; clear H
         end
  | 2 => lazymatch goal with
         | |- reduce _ _ _ ([::$VN ?c1] ++ [::$VN ?c2] ++ [:: ?instr])        _ _ _ _ => idtac
         | |- reduce _ _ _ ([::$VN ?c1] ++ [::$VN ?c2] ++ [:: ?instr] ++ ?l3) _ _ _ _ =>
            assert ([::$VN c1] ++ [:: $VN c2] ++ [:: instr] ++ l3 =
                    [::$VN c1; $VN c2; instr] ++ l3) as H by reflexivity; rewrite H;
                                                       apply r_elimr; clear H
         end
  end.

(** One reduction step, leaving a tail of instructions. *)
Ltac dostep :=
  eapply rt_trans with (y := (?[hs], ?[sr], ?[f'], ?[s] ++ ?[t]));
  first (apply rt_step; separate_instr).

(** One reduction step of the whole instruction list. *)
Ltac dostep' :=
   eapply rt_trans with (y := (?[hs], ?[sr], ?[f'], ?[s]));
   first (apply rt_step; separate_instr).

Ltac dostep_nary n :=
  dostep; first elimr_nary_instr n.

Ltac dostep_nary' n :=
  dostep'; first elimr_nary_instr n.

(** As [dostep_nary], but leaving [n'] leading values untouched. *)
Ltac dostep_nary_eliml n n' :=
  dostep; first ((do n'! (apply r_eliml; auto)); elimr_nary_instr n).

(** ** Stepping without naming the step

    [dostep_nary n] has to be told two things the goal already determines:
    how many values the instruction at the head consumes, and how many
    values are sitting in front of it.  [dostep_auto] finds both and fires
    the rule for that instruction; [dostep_all] repeats it until it reaches
    an instruction it does not handle.

    It handles the *pure* instructions — the ones whose step is a function of
    the operand stack and the frame alone.  Loads, stores, calls, [if] and
    branches are left alone deliberately: each of those needs a fact about
    the store that only the surrounding proof has (which [smem_store] result,
    which [sglob_val]), and firing them automatically would mean an [eauto]
    over the store lemmas whose failures are far harder to read than a
    [dostep_nary] that stopped. [local.set] is excluded for a related reason:
    its rule needs the *target frame*, and these proofs keep frames in a
    canonical form ([exp_locs] in the example files) that no tactic can
    guess — hence [dostep_set] below, which takes it as its one argument.

    So an adapter proof becomes [dostep_all] runs separated by the handful of
    steps that are genuinely about the store.  The instruction list has to be
    in explicit [AI_*] form already (the [cbn] that [dostep] performs is
    inside the step, too late to dispatch on); every adapter proof starts
    with that [cbn] anyway. *)

(** The rules, tried in order.  Nothing here searches: each alternative is
    one [apply] whose side conditions are either a [lookup_N] on a concrete
    frame, a [*_typecheck] boolean, or an [app_binop] — all [reflexivity] —
    or a [sglob_val] that the context has to supply. *)
Ltac pure_step_rule :=
  first
    [ apply r_local_get; reflexivity
    | apply r_global_get;
      first [ eassumption
            | match goal with H : f_inst _ = _ |- _ => rewrite H; eassumption end ]
    | apply r_simple; eapply rs_unop; reflexivity
    | apply r_simple; eapply rs_binop_success; [reflexivity | reflexivity]
    | apply r_simple; eapply rs_relop; reflexivity
    | apply r_simple; apply rs_testop_i32
    | apply r_simple; apply rs_testop_i64 ].

(** The split, found by trying the possibilities rather than by reading the
    instruction list.  Each alternative is a [dostep_*] *paired with the
    rule*, which is what makes the search sound: a split that grouped the
    wrong instructions — [elimr_nary_instr] will happily shift its pattern
    along a run of values, taking a constant for the instruction — leaves a
    [reduce] goal no rule applies to, so the branch fails and is backtracked
    over.  The one that both splits and steps is the right one.

    Searching rather than dispatching is not elegant, but dispatching needs
    arithmetic on the arity (how many values to leave in front = how many are
    on the stack minus what the instruction takes) and Ltac1 has none: a
    numeral is either an [int] it cannot subtract with or a [constr] the
    [dostep_*] tactics do not take.  The pairs below cover arity up to 2 with
    up to three values in front, which is every instruction a canonical-ABI
    adapter contains.

    Ordered cheapest-first; within that, the shapes an adapter body actually
    produces come before the rest. *)
Ltac dostep_auto :=
  first [ dostep_nary 0          ; [pure_step_rule | ]
        | dostep_nary 1          ; [pure_step_rule | ]
        | dostep_nary 2          ; [pure_step_rule | ]
        | dostep_nary_eliml 0 1  ; [pure_step_rule | ]
        | dostep_nary_eliml 1 1  ; [pure_step_rule | ]
        | dostep_nary_eliml 2 1  ; [pure_step_rule | ]
        | dostep_nary_eliml 0 2  ; [pure_step_rule | ]
        | dostep_nary_eliml 1 2  ; [pure_step_rule | ]
        | dostep_nary_eliml 2 2  ; [pure_step_rule | ]
        | dostep_nary_eliml 0 3  ; [pure_step_rule | ]
        | dostep_nary' 0         ; [pure_step_rule | ]
        | dostep_nary' 1         ; [pure_step_rule | ]
        | dostep_nary' 2         ; [pure_step_rule | ] ].

Ltac dostep_all := repeat dostep_auto.

(** [local.set i], with the frame it produces.  The three side conditions of
    [r_local_set] are the same three every time: the instance is unchanged,
    the index is in range, and the new locals are the old ones updated. *)
Ltac dostep_set i f' :=
  dostep_nary 1;
  [ eapply r_local_set with (i := i) (f' := f')
      (vd := VAL_num (VAL_int32 (Wasm_int.Int32.repr 0)));
    [ reflexivity | cbn; (reflexivity || lia) | cbn; reflexivity ]
  | ].
