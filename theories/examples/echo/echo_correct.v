(** * The echo adapter implements [ffi_echo]

    This is the statement the slice exists to make: the hand-written Wasm in
    [echo_adapter.v] realises the registered CertiRocq primitive

      Axiom ffi_echo : bytes -> option bytes.

    relative to the heap representation of [echo_heap.v], the canonical ABI of
    [echo_abi.v], and one assumption about the provider.

    The shape of [ffi_echo_correct] below is the slice's instance of the
    *primitive import contract* (PLAN-authentikate.md §E(3)): terminates,
    preserves the backend's invariant, publishes the represented result in
    [glob_result], and frames everything below the allocation pointer it was
    entered with.  That is what the backend's [Eprim] case needs in order to
    drop [Hypothesis Hno_prim_imports], so the statement here is the interface
    between the two halves of the project — and it is why the frame clause,
    not the functional clause, is the load-bearing one.

    ** Status

    Proved: [alloc_spec] (straight-line plus the [memory.grow] branch),
    [list_len_spec] (the spine walk) and [some_spec] (the first helper that
    writes to memory).  Between them they are the template for the rest: the
    numeric content is isolated into the [app_binop_*] lemmas, the two ways
    out of a [block]/[loop] pair into [br_block_loop] / [br_loop_continue],
    the memory writes into [echo_mem.v] and [store_i32_*] in [echo_heap.v],
    and what is left is an induction plus a straight-line prologue and
    epilogue around it.

    Still [Admitted], in the order they should be discharged:

    - [lift_bytes_spec]  — second loop; builds the spine back to front
    - [lower_spec]       — third loop; needs [alloc_spec] and [list_len_spec]
    - [ffi_echo_correct] — composes the above with the provider assumption

    Both loop specifications still carry the statements they were sketched
    with, and both are false as written for the same two reasons [some_spec]
    was: they call other functions by index, so they need the [native_at]
    obligations that say those indices resolve; and they allocate, so they
    need more address space than [ai_no_wrap] promises — a bound in the
    length of the data, since that is how much they allocate.  Fixing the
    statements is part of discharging them, as it was for [some_spec]. *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics memory opsem host properties.

From Stdlib Require Import ZArith BinNat NArith.Nnat List Strings.String Strings.Byte Lia
  Relations.Relations Relations.Relation_Operators.

From WasmGlue Require Import wasm_steps wasm_mem certirocq_heap certirocq_inv.
From wasmcomponents Require Import echo_heap echo_abi echo_adapter.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.


Section Correctness.

Context `{ho : host}.


Definition ty_echo : function_type :=
  Tf [T_num T_i32; T_num T_i32; T_num T_i32] [].

Record adapter_instance (sr : store_record) (inst : moduleinst)
                        (hecho : host_function) : Prop := {
  aei_echo       : host_at sr inst fidx_echo ty_echo hecho;
  aei_alloc      : native_at sr inst fidx_alloc
                     (Tf [T_num T_i32; T_num T_i32] [T_num T_i32]) alloc_func;
  aei_list_len   : native_at sr inst fidx_list_len
                     (Tf [T_num T_i32] [T_num T_i32]) list_len_func;
  aei_lower      : native_at sr inst fidx_lower
                     (Tf [T_num T_i32] [T_num T_i32]) lower_func;
  aei_lift_bytes : native_at sr inst fidx_lift_bytes
                     (Tf [T_num T_i32; T_num T_i32] [T_num T_i32]) lift_bytes_func;
  aei_some       : native_at sr inst fidx_some
                     (Tf [T_num T_i32] [T_num T_i32]) some_func;
  aei_ffi_echo   : native_at sr inst fidx_ffi_echo
                     (Tf [T_num T_i32] [T_num T_i32]) ffi_echo_func;
  aei_export     : native_at sr inst fidx_export
                     (Tf [T_num T_i32; T_num T_i32] []) export_func
}.

(** ** The provider assumption

    The single hypothesis about the outside world, stated over the canonical
    ABI (PLAN-authentikate.md §H(1)).  Given an argument buffer, the provider
    fills the return area with its answer, moves the allocation pointer only
    upwards, and touches nothing below the pointer except the return area it
    was handed.

    Two things it deliberately does not promise: that the answer is related to
    the argument in any way (the adapter's correctness is relative to whatever
    [echo_impl] is), and that it never fails — the [err] arm is reachable and
    becomes [None]. *)
Definition provider_frames (m m' : meminst) (gmp ret : N) : Prop :=
  forall a, (a + 4 <= gmp)%N ->
            ((a + 4 <= ret)%N \/ (ret + result_bytes_size <= a)%N) ->
            load_i32 m' a = load_i32 m a.

Definition echo_provider_spec (prov : echo_provider) (hecho : host_function)
                              (inst : moduleinst) : Prop :=
  forall hs sr m gmp p ret bs,
    smem sr inst = Some m ->
    sglob_val sr inst glob_mem_ptr = Some (N_to_value gmp) ->
    buffer_at m p bs ->
    (ret + result_bytes_size <= gmp)%N ->
    exists hs' sr' m' gmp',
      host_application hs sr ty_echo hecho
        [N_to_value p; N_to_value (N.of_nat (List.length bs)); N_to_value ret]
        hs' (Some (sr', result_values [])) /\
      smem sr' inst = Some m' /\
      sglob_val sr' inst glob_mem_ptr = Some (N_to_value gmp') /\
      (gmp <= gmp')%N /\
      sglob_val sr' inst glob_out_of_mem = Some (N_to_value 0) /\
      canon_result_bytes m' ret (echo_impl prov bs) /\
      provider_frames m m' gmp ret.


(** ** Specifications of the helper functions

    Each is the contract its callers rely on.  [alloc_spec] is stated with an
    explicit disjunction rather than a precondition ruling out failure,
    because the adapter's whole error protocol is built on the failing case
    being reachable and observable. *)

(** *** The straight-line head of [$alloc] *)

(** The six locals of [$alloc]: the two parameters [n], [align] and the four
    scratch locals [p], [end], [have], [pages]. *)
Definition alloc_locs (n align p en hv pg : N) (inst : moduleinst) : frame :=
  Build_frame [:: N_to_value n; N_to_value align; N_to_value p;
                  N_to_value en; N_to_value hv; N_to_value pg] inst.

Arguments alloc_locs : simpl never.
Arguments wasm_bool : simpl never.

(** Everything up to the overflow test, in one go: the head is straight-line,
    so this is 23 reduction steps and no case analysis.  [rest] is whatever
    follows in the function body — the [if] and the epilogue — left untouched
    by [r_elimr]. *)
Lemma alloc_head_spec : forall hs sr inst gmp n align mi hv p b rest,
  sglob_val sr inst glob_mem_ptr = Some (N_to_value gmp) ->
  smem sr inst = Some mi ->
  (align = 1 \/ align = 4 \/ align = 8)%N ->
  (gmp + align + n + 12 < i32mod)%N ->
  app_binop (Binop_i BOI_shl)
    (VAL_int32 (N_to_i32 (mem_size mi))) (VAL_int32 (N_to_i32 16))
    = Some (VAL_int32 (N_to_i32 hv)) ->
  (hv < i32mod)%N ->
  p = alloc_ptr gmp align ->
  (hv <? p + n)%N = b ->
  reduce_trans
    (hs, sr, alloc_locs n align 0 0 0 0 inst, to_e_list alloc_head ++ rest)
    (hs, sr, alloc_locs n align p (p + n) hv 0 inst,
     [:: AI_basic (BI_const_num (VAL_int32 (wasm_bool b)))] ++ rest).
Proof.
  intros hs sr inst gmp n align mi hv p b rest Hgmp Hmem Hal Hwrap Hshl Hhv Hp Hb.
  subst p.
  assert (Hal1 : (1 <= align)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  assert (Hal8 : (align <= 8)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  have [[Hplo Hphi] _] := alloc_ptr_bounds gmp align Hal1.
  assert (Hpn : (alloc_ptr gmp align + n < i32mod)%N) by lia.
  cbn. unfold i32c, i32_minus_one, add, sub, and_, xor, shl, gt_u.
  (* p := (mem_ptr + (align - 1)) & ~(align - 1) *)
  dostep_nary 0. apply r_global_get. exact Hgmp.
  dostep_nary_eliml 0 1. apply r_local_get. reflexivity.
  dostep_nary_eliml 2 1. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_sub; lia].
  dostep_nary 2. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_add].
  dostep_nary_eliml 0 1. apply r_local_get. reflexivity.
  dostep_nary_eliml 2 1. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_sub; lia].
  dostep_nary_eliml 2 1. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_xor].
  dostep_nary 2. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_align_up; [exact Hal | lia]].
  dostep_nary 1.
  { eapply r_local_set with
      (v := N_to_value (alloc_ptr gmp align))
      (f' := alloc_locs n align (alloc_ptr gmp align) 0 0 0 inst)
      (vd := N_to_value 0);
      [reflexivity | unfold alloc_locs; cbn; reflexivity
       | unfold alloc_locs; cbn; reflexivity]. }
  (* end := p + n *)
  dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
  dostep_nary_eliml 0 1. apply r_local_get. unfold alloc_locs. reflexivity.
  dostep_nary 2. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply app_binop_add].
  dostep_nary 1.
  { eapply r_local_set with
      (v := N_to_value (alloc_ptr gmp align + n))
      (f' := alloc_locs n align (alloc_ptr gmp align) (alloc_ptr gmp align + n) 0 0 inst)
      (vd := N_to_value 0);
      [reflexivity | unfold alloc_locs; cbn; reflexivity
       | unfold alloc_locs; cbn; reflexivity]. }
  (* have := memory.size * 64Ki *)
  dostep_nary 0. eapply r_memory_size; [exact Hmem | reflexivity].
  dostep_nary 2. apply r_simple.
  eapply rs_binop_success; [reflexivity | apply Hshl].
  dostep_nary 1.
  { eapply r_local_set with
      (v := N_to_value hv)
      (f' := alloc_locs n align (alloc_ptr gmp align) (alloc_ptr gmp align + n) hv 0 inst)
      (vd := N_to_value 0);
      [reflexivity | unfold alloc_locs; cbn; reflexivity
       | unfold alloc_locs; cbn; reflexivity]. }
  (* end >u have *)
  dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
  dostep_nary_eliml 0 1. apply r_local_get. unfold alloc_locs. reflexivity.
  dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
  subst b. rewrite (app_relop_gt_u _ hv Hpn Hhv). apply rt_refl.
Qed.


Lemma to_e_list_alloc_body :
  to_e_list alloc_body
  = (to_e_list alloc_head
     ++ [:: AI_basic (BI_if (BT_valtype None) alloc_grow [::])]
     ++ to_e_list alloc_tail).
Proof. reflexivity. Qed.

(** *** Entering [$alloc]

    [call] then [invoke] then the function's own [block], then
    [alloc_head_spec] — the part of the run that is the same whatever the
    overflow test says. *)
Lemma alloc_entry : forall hs sr inst fr gmp n align fa mi hv b,
  lookup_N (inst_funcs inst) fidx_alloc = Some fa ->
  lookup_N (s_funcs sr) fa
    = Some (FC_func_native (Tf [:: T_num T_i32; T_num T_i32] [:: T_num T_i32])
                           inst alloc_func) ->
  f_inst fr = inst ->
  sglob_val sr inst glob_mem_ptr = Some (N_to_value gmp) ->
  smem sr inst = Some mi ->
  (align = 1 \/ align = 4 \/ align = 8)%N ->
  (gmp + align + n + 12 < i32mod)%N ->
  app_binop (Binop_i BOI_shl)
    (VAL_int32 (N_to_i32 (mem_size mi))) (VAL_int32 (N_to_i32 16))
    = Some (VAL_int32 (N_to_i32 hv)) ->
  (hv < i32mod)%N ->
  (hv <? alloc_ptr gmp align + n)%N = b ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_basic (i32c n); AI_basic (i32c align); AI_basic (BI_call fidx_alloc)])
    (hs, sr, fr,
     [:: AI_frame 1 (alloc_locs n align (alloc_ptr gmp align)
                                (alloc_ptr gmp align + n) hv 0 inst)
           [:: AI_label 1 [::]
                 ([:: AI_basic (BI_const_num (VAL_int32 (wasm_bool b)))]
                  ++ [:: AI_basic (BI_if (BT_valtype None) alloc_grow [::])]
                  ++ to_e_list alloc_tail)]]).
Proof.
  intros hs sr inst fr gmp n align fa mi hv b
         Hidx Hfunc Hfinst Hgmp Hmem Hal Hwrap Hshl Hhv Hb.
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_basic (i32c n); AI_basic (i32c align); AI_invoke fa])).
  { apply rt_step.
    eapply r_eliml with (lconst := [:: AI_basic (i32c n); AI_basic (i32c align)]);
      [reflexivity|].
    apply r_call. rewrite Hfinst. exact Hidx. }
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_frame 1 (alloc_locs n align 0 0 0 0 inst)
          [:: AI_label 1 [::] (to_e_list alloc_body)]])).
  { apply rt_step.
    eapply r_invoke_native with
      (cl := FC_func_native (Tf [:: T_num T_i32; T_num T_i32] [:: T_num T_i32])
                            inst alloc_func)
      (ts1 := [:: T_num T_i32; T_num T_i32]) (ts2 := [:: T_num T_i32])
      (code := alloc_func) (x := ty_i32i32_i32)
      (ts := repeat (T_num T_i32) 4) (es := alloc_body)
      (ves := [:: AI_basic (i32c n); AI_basic (i32c align)])
      (vs := [:: N_to_value n; N_to_value align])
      (n := 2) (m := 1) (k := 4)
      (defaults := [:: N_to_value 0; N_to_value 0; N_to_value 0; N_to_value 0])
      (inst := inst);
      try reflexivity.
    exact Hfunc. }
  rewrite to_e_list_alloc_body.
  apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
  eapply alloc_head_spec; (eassumption || reflexivity).
Qed.



(** *** Leaving [$alloc]

    [mem_ptr := end; return p], then out through the [block] label and the
    frame.  Shared by the two arms that succeed. *)
Lemma alloc_tail_spec : forall hs sr sr' inst fr n align p en hv pg,
  supdate_glob sr inst glob_mem_ptr (N_to_value en) = Some sr' ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_frame 1 (alloc_locs n align p en hv pg inst)
           [:: AI_label 1 [::] (to_e_list alloc_tail)]])
    (hs, sr', fr, [:: AI_basic (i32c p)]).
Proof.
  intros hs sr sr' inst fr n align p en hv pg Hupd.
  eapply rt_trans with (y := (hs, sr', fr,
    [:: AI_frame 1 (alloc_locs n align p en hv pg inst)
          [:: AI_label 1 [::] [:: AI_basic (i32c p)]]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    cbn. unfold i32c.
    dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
    dostep_nary 1. eapply r_global_set with (v := N_to_value en). exact Hupd.
    apply rt_step. eapply r_local_get with (v := N_to_value p).
    unfold alloc_locs. reflexivity. }
  eapply rt_trans with (y := (hs, sr', fr,
    [:: AI_frame 1 (alloc_locs n align p en hv pg inst) [:: AI_basic (i32c p)]])).
  { apply (reduce_trans_frame 1 fr). apply rt_step. apply r_simple.
    apply rs_label_const. reflexivity. }
  apply rt_step. apply r_simple. eapply rs_local_const; reflexivity.
Qed.

(** *** The arm that does not grow *)
Lemma alloc_nogrow_spec : forall hs sr sr' inst fr n align p en hv,
  supdate_glob sr inst glob_mem_ptr (N_to_value en) = Some sr' ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
           [:: AI_label 1 [::]
                 ([:: AI_basic (BI_const_num (VAL_int32 (wasm_bool false)))]
                  ++ [:: AI_basic (BI_if (BT_valtype None) alloc_grow [::])]
                  ++ to_e_list alloc_tail)]])
    (hs, sr', fr, [:: AI_basic (i32c p)]).
Proof.
  intros hs sr sr' inst fr n align p en hv Hupd.
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
          [:: AI_label 1 [::] (to_e_list alloc_tail)]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    dostep_nary 1. apply r_simple. apply rs_if_false. reflexivity.
    dostep_nary 0.
    eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
      reflexivity.
    dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
    apply rt_refl. }
  apply alloc_tail_spec. exact Hupd.
Qed.

(** *** The arm that grows the memory and succeeds *)
Lemma alloc_grow_ok_spec : forall hs sr sr2 sr3 inst fr n align p en hv pg sz,
  (hv <= en)%N -> (en < i32mod)%N -> (pg < i32mod)%N -> (sz + 1 < i32mod)%N ->
  pg = ((en - hv) / 65536 + 1)%N ->
  smem_grow sr inst pg = Some (sr2, sz) ->
  supdate_glob sr2 inst glob_mem_ptr (N_to_value en) = Some sr3 ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
           [:: AI_label 1 [::]
                 ([:: AI_basic (BI_const_num (VAL_int32 (wasm_bool true)))]
                  ++ [:: AI_basic (BI_if (BT_valtype None) alloc_grow [::])]
                  ++ to_e_list alloc_tail)]])
    (hs, sr3, fr, [:: AI_basic (i32c p)]).
Proof.
  intros hs sr sr2 sr3 inst fr n align p en hv pg sz
         Hhe Hen Hpg Hsz Hpgdef Hgrow Hupd.
  subst pg.
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
          [:: AI_label 1 [::]
                ([:: AI_label 0 [::] (to_e_list alloc_grow)]
                 ++ to_e_list alloc_tail)]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    dostep_nary 1. apply r_simple. apply rs_if_true_bool. reflexivity.
    dostep_nary 0.
    eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
      reflexivity.
    apply rt_refl. }
  eapply rt_trans with (y := (hs, sr2, fr,
    [:: AI_frame 1 (alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst)
          [:: AI_label 1 [::] (to_e_list alloc_tail)]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    eapply rt_trans with (y := (hs, sr2,
      alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst,
      [:: AI_label 0 [::] [::]] ++ to_e_list alloc_tail)).
    { apply (reduce_trans_elimr (to_e_list alloc_tail)).
      apply reduce_trans_label0.
      cbn. unfold i32c, i32_minus_one, sub, add, shr_u, eq_.
      dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary_eliml 0 1. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_sub; lia].
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_shr_u_16; lia].
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_add].
      dostep_nary 1.
      { eapply r_local_set with
          (v := N_to_value ((en - hv) / 65536 + 1))
          (f' := alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst)
          (vd := N_to_value 0);
          [reflexivity | unfold alloc_locs; cbn; reflexivity
           | unfold alloc_locs; cbn; reflexivity]. }
      dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary 1.
      { eapply r_memory_grow_success.
        rewrite (N_of_uint_N_to_i32 _ Hpg). exact Hgrow. }
      dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
      dostep_nary' 1. apply r_simple. cbn. apply rs_if_false_bool.
      apply app_relop_eq_mone_false. exact Hsz.
      dostep_nary' 0.
      eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
        reflexivity.
      apply rt_step. apply r_simple. apply rs_label_const. reflexivity. }
    dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
    apply rt_refl. }
  apply alloc_tail_spec. exact Hupd.
Qed.

(** *** The arm that grows the memory and fails

    [memory.grow] returning [-1] is the adapter's only error path: it sets
    [glob_out_of_mem] and returns [0] through [BI_return], which unwinds three
    labels at once — the function's, the outer [if]'s and the inner [if]'s. *)
Lemma alloc_grow_fail_spec : forall hs sr sr2 inst fr n align p en hv pg,
  (hv <= en)%N -> (en < i32mod)%N -> (pg < i32mod)%N ->
  pg = ((en - hv) / 65536 + 1)%N ->
  smem_grow sr inst pg = None ->
  supdate_glob sr inst glob_out_of_mem (N_to_value 1) = Some sr2 ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
           [:: AI_label 1 [::]
                 ([:: AI_basic (BI_const_num (VAL_int32 (wasm_bool true)))]
                  ++ [:: AI_basic (BI_if (BT_valtype None) alloc_grow [::])]
                  ++ to_e_list alloc_tail)]])
    (hs, sr2, fr, [:: AI_basic (i32c 0)]).
Proof.
  intros hs sr sr2 inst fr n align p en hv pg Hhe Hen Hpg Hpgdef Hgrow Hupd.
  subst pg.
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_frame 1 (alloc_locs n align p en hv 0 inst)
          [:: AI_label 1 [::]
                ([:: AI_label 0 [::] (to_e_list alloc_grow)]
                 ++ to_e_list alloc_tail)]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    dostep_nary 1. apply r_simple. apply rs_if_true_bool. reflexivity.
    dostep_nary 0.
    eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
      reflexivity.
    apply rt_refl. }
  eapply rt_trans with (y := (hs, sr2, fr,
    [:: AI_frame 1 (alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst)
          [:: AI_label 1 [::]
                ([:: AI_label 0 [::]
                       [:: AI_label 0 [::]
                             ([:: AI_basic (i32c 0)] ++ [:: AI_basic BI_return])]]
                 ++ to_e_list alloc_tail)]])).
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    apply (reduce_trans_elimr (to_e_list alloc_tail)).
    apply reduce_trans_label0.
    eapply rt_trans with (y := (hs, sr,
      alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst,
      [:: AI_basic (BI_block (BT_valtype None)
             [:: i32c 1; BI_global_set glob_out_of_mem; i32c 0; BI_return])])).
    { cbn. unfold i32c, i32_minus_one, sub, add, shr_u, eq_.
      dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary_eliml 0 1. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_sub; lia].
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_shr_u_16; lia].
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_add].
      dostep_nary 1.
      { eapply r_local_set with
          (v := N_to_value ((en - hv) / 65536 + 1))
          (f' := alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst)
          (vd := N_to_value 0);
          [reflexivity | unfold alloc_locs; cbn; reflexivity
           | unfold alloc_locs; cbn; reflexivity]. }
      dostep_nary 0. apply r_local_get. unfold alloc_locs. reflexivity.
      dostep_nary 1.
      { eapply r_memory_grow_failure.
        rewrite (N_of_uint_N_to_i32 _ Hpg). exact Hgrow. }
      dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
      apply rt_step. apply r_simple. cbn. apply rs_if_true_bool.
      apply app_relop_eq_mone_true. }
    eapply rt_trans with (y := (hs, sr,
      alloc_locs n align p en hv ((en - hv) / 65536 + 1) inst,
      [:: AI_label 0 [::]
            (to_e_list [:: i32c 1; BI_global_set glob_out_of_mem;
                           i32c 0; BI_return])])).
    { apply rt_step.
      eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
        reflexivity. }
    apply reduce_trans_label0.
    cbn. unfold i32c.
    dostep_nary 1. eapply r_global_set with (v := N_to_value 1). exact Hupd.
    apply rt_refl. }
  apply rt_step. apply r_simple.
  eapply rs_return with
    (n := 1) (vs := [:: AI_basic (i32c 0)])
    (lh := LH_rec [::] 1 [::]
             (LH_rec [::] 0 [::]
                (LH_rec [::] 0 [::] (LH_base [::] [::]) [::])
                (to_e_list alloc_tail)) [::]);
    reflexivity.
Qed.

(** *** [$alloc]

    The contract its callers rely on, with the disjunction stated rather than
    a precondition ruling out failure: the adapter's whole error protocol is
    built on [memory.grow] failing being reachable and observable.

    The success arm does not re-establish [adapter_inv] but the weaker
    [alloc_inv], for the reason given there: [$alloc n 1] with odd [n] leaves
    the allocation pointer odd.  Callers recover evenness from
    [alloc_ptr_bounds], which also gives [gmp <= alloc_ptr gmp align <= gmp +
    align - 1] — stated separately because it is pure arithmetic and holds
    whichever arm was taken. *)
Lemma alloc_spec : forall hs sr inst fr gmp n align,
  alloc_inv sr inst gmp ->
  native_at sr inst fidx_alloc
    (Tf [T_num T_i32; T_num T_i32] [T_num T_i32]) alloc_func ->
  (align = 1%N \/ align = 4%N \/ align = 8%N) ->
  (gmp + align + n + 12 < i32mod)%N ->
  f_inst fr = inst ->
  exists sr' res,
    reduce_trans
      (hs, sr, fr,
       [AI_basic (i32c n); AI_basic (i32c align); AI_basic (BI_call fidx_alloc)])
      (hs, sr', fr, [AI_basic (i32c res)]) /\
    ( (exists mi mi',
          res = alloc_ptr gmp align /\
          smem sr inst = Some mi /\ smem sr' inst = Some mi' /\
          alloc_inv sr' inst (alloc_ptr gmp align + n) /\
          mem_agree_below mi mi' gmp)
      \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).
Proof.
  intros hs sr inst fr gmp n align Hinv Hnat Hal Hwrap Hfinst.
  destruct Hnat as [fa [Hidx Hfunc]].
  have Hgmp := aai_gmp _ _ _ Hinv.
  have Hoom := aai_oom _ _ _ Hinv.
  have Hdist := aai_glob_distinct _ _ _ Hinv.
  have [mi [Hmem Hmle]] := aai_mem _ _ _ Hinv.
  assert (Hal1 : (1 <= align)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  assert (Hal8 : (align <= 8)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  have [[Hplo Hphi] Hdiv] := alloc_ptr_bounds gmp align Hal1.
  have [hv [Hshl [Hhle Hhlt]]] := mem_size_shl mi.
  assert (Hen : (alloc_ptr gmp align + n < i32mod)%N) by lia.
  assert (Hnw : (alloc_ptr gmp align + n + 12 < i32mod)%N) by lia.
  have Hps : page_size = 65536%N. { reflexivity. }
  destruct ((hv <? alloc_ptr gmp align + n)%N) eqn:Hcmp.
  - (* the allocation does not fit; grow *)
    assert (Hlt : (hv < alloc_ptr gmp align + n)%N) by (apply N.ltb_lt; exact Hcmp).
    assert (Hq : ((alloc_ptr gmp align + n - hv) / 65536 < 65536)%N).
    { apply N.Div0.div_lt_upper_bound. rewrite i32mod_val in Hen. lia. }
    assert (Hpgb : ((alloc_ptr gmp align + n - hv) / 65536 + 1 < i32mod)%N)
      by (rewrite i32mod_val; lia).
    destruct (smem_grow sr inst ((alloc_ptr gmp align + n - hv) / 65536 + 1))
      as [[sr2 sz]|] eqn:Hgrow.
    + (* grown *)
      have [mi' [Hmem2 [Hlen2 [Hlim2 [Hagree2 [Hsz [Hglob2 Hfun2]]]]]]] :=
        smem_grow_spec _ _ _ _ _ _ Hmem Hgrow.
      subst sz.
      assert (Hszb : (mem_size mi + 1 < i32mod)%N).
      { unfold mem_limit_bound in Hlim2. rewrite i32mod_val.
        revert Hlim2. generalize (mem_size mi) as MS.
        generalize ((alloc_ptr gmp align + n - hv) / 65536)%N as QQ.
        clear. intros. lia. }
      assert (Hgmp2 : sglob_val sr2 inst glob_mem_ptr = Some (N_to_value gmp))
        by (rewrite (sglob_val_globals sr sr2 inst glob_mem_ptr Hglob2); exact Hgmp).
      have [sr3 Hupd] := supdate_glob_exists sr2 inst glob_mem_ptr
                           (N_to_value (alloc_ptr gmp align + n)) _ Hgmp2.
      exists sr3, (alloc_ptr gmp align). split.
      { eapply rt_trans.
        { eapply alloc_entry; (eassumption || reflexivity). }
        eapply alloc_grow_ok_spec with
          (sr2 := sr2) (sz := mem_size mi)
          (pg := ((alloc_ptr gmp align + n - hv) / 65536 + 1)%N);
          (eassumption || reflexivity || lia). }
      left. exists mi, mi'.
      split; [reflexivity|].
      split; [exact Hmem|].
      split; [rewrite (supdate_glob_mem _ _ _ _ _ Hupd); exact Hmem2|].
      split.
      { constructor.
        - apply (supdate_glob_same _ _ _ _ _ Hupd).
        - eapply supdate_glob_other with (sr := sr2); [exact Hdist | | exact Hupd].
          rewrite (sglob_val_globals sr sr2 inst glob_out_of_mem Hglob2). exact Hoom.
        - exists mi'.
          split; [rewrite (supdate_glob_mem _ _ _ _ _ Hupd); exact Hmem2|].
          rewrite Hps in Hlen2.
          have Hdm := N.Div0.div_mod (alloc_ptr gmp align + n - hv) 65536.
          have Hmd : ((alloc_ptr gmp align + n - hv) mod 65536 < 65536)%N
            by (apply N.mod_lt; lia).
          lia.
        - exact Hnw.
        - exact Hdist. }
      eapply mem_agree_below_le; [exact Hagree2 | lia].
    + (* out of memory *)
      have [sr2 Hupd] := supdate_glob_exists sr inst glob_out_of_mem
                           (N_to_value 1) _ Hoom.
      exists sr2, 0%N. split.
      { eapply rt_trans.
        { eapply alloc_entry; (eassumption || reflexivity). }
        eapply alloc_grow_fail_spec with
          (pg := ((alloc_ptr gmp align + n - hv) / 65536 + 1)%N);
          (eassumption || reflexivity || lia). }
      right. apply (supdate_glob_same _ _ _ _ _ Hupd).
  - (* it fits as it is *)
    assert (Hge : (alloc_ptr gmp align + n <= hv)%N) by (apply N.ltb_ge; exact Hcmp).
    have [sr1 Hupd] := supdate_glob_exists sr inst glob_mem_ptr
                         (N_to_value (alloc_ptr gmp align + n)) _ Hgmp.
    exists sr1, (alloc_ptr gmp align). split.
    { eapply rt_trans.
      { eapply alloc_entry; (eassumption || reflexivity). }
      apply alloc_nogrow_spec. exact Hupd. }
    left. exists mi, mi.
    split; [reflexivity|].
    split; [exact Hmem|].
    split; [rewrite (supdate_glob_mem _ _ _ _ _ Hupd); exact Hmem|].
    split.
    { constructor.
      - apply (supdate_glob_same _ _ _ _ _ Hupd).
      - eapply supdate_glob_other; [exact Hdist | exact Hoom | exact Hupd].
      - exists mi.
        split; [rewrite (supdate_glob_mem _ _ _ _ _ Hupd); exact Hmem | lia].
      - exact Hnw.
      - exact Hdist. }
    intros addr Haddr. reflexivity.
Qed.

(** *** The spine walk

    The loop invariant, stated as the whole run rather than as an invariant:
    entered with the spine pointer in local 0 and a counter in local 1, the
    [block]/[loop] pair terminates with the counter advanced by the length of
    the spine and the pointer at the [EmptyString] immediate.

    The induction is on the derivation of [repr_bytes], not on [bs]: it is the
    derivation that supplies the next cell's address and the bound that keeps
    it inside i32 range. *)
Lemma list_len_loop_spec : forall bs hs sr inst m gmp l n,
  smem sr inst = Some m ->
  (gmp + 12 < i32mod)%N ->
  repr_bytes m gmp l bs ->
  reduce_trans
    (hs, sr, Build_frame [:: N_to_value l; N_to_value n] inst,
     [:: AI_label 0 [::] [:: AI_basic (BI_loop (BT_valtype None) list_len_loop)]])
    (hs, sr, Build_frame [:: N_to_value (unboxed 0);
                             N_to_value (n + N.of_nat (List.length bs))] inst,
     [::]).
Proof.
  intros bs hs sr inst m gmp l n Hmem Hwrap Hrepr. revert n.
  induction Hrepr as [| addr b bs' tail Hbound Heven Htag Hhd Htl Hrec IH]; intros n.
  - (* the spine ends: the tag bit is set, so [br_if 1] leaves the block *)
    cbn [List.length N.of_nat]. rewrite N.add_0_r.
    assert (Hu : unboxed 0 = 1%N) by reflexivity. rewrite Hu.
    have Hone := one_lt_i32mod.
    eapply rt_trans.
    { apply reduce_trans_label0. apply rt_step.
      eapply r_loop with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
        reflexivity. }
    eapply rt_trans.
    { apply reduce_trans_label0.
      apply (reduce_trans_label_cont 0
               [:: AI_basic (BI_loop (BT_valtype None) list_len_loop)]).
      (* [cbn] expands the body into an instruction list but leaves the opcode
         abbreviations of [echo_adapter.v] folded, and [elimr_nary_instr]
         matches on [BI_const_num] syntactically. *)
      cbn. unfold i32c, and_, add, load_word.
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_and_one; lia].
      dostep_nary 1. apply r_simple. apply rs_br_if_true.
      cbn. apply N_to_i32_one_neq_zero.
      apply rt_refl. }
    apply br_block_loop.
  - (* one cell: fall through, bump the counter, follow the tail pointer *)
    assert (Haddr : (addr < i32mod)%N) by lia.
    assert (Hmod2 : (addr mod 2 = 0)%N)
      by (destruct Heven as [k ->]; rewrite N.mul_comm; apply N.Div0.mod_mul; lia).
    replace (n + N.of_nat (List.length (b :: bs')))%N
       with (n + 1 + N.of_nat (List.length bs'))%N
       by (cbn [List.length]; rewrite Nat2N.inj_succ; lia).
    (* the tail pointer, as [r_load_success] will read it *)
    unfold load_i32 in Htl.
    destruct (load m (addr + 8)%N 0%N 4) as [bstail|] eqn:Hload; [|discriminate].
    assert (Htl' : wasm_deserialise bstail T_i32 = VAL_int32 (N_to_i32 tail))
      by congruence.
    clear Htl.
    eapply rt_trans.
    { apply reduce_trans_label0. apply rt_step.
      eapply r_loop with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
        reflexivity. }
    eapply rt_trans.
    { apply reduce_trans_label0.
      apply (reduce_trans_label_cont 0
               [:: AI_basic (BI_loop (BT_valtype None) list_len_loop)]).
      (* [cbn] expands the body into an instruction list but leaves the opcode
         abbreviations of [echo_adapter.v] folded, and [elimr_nary_instr]
         matches on [BI_const_num] syntactically. *)
      cbn. unfold i32c, and_, add, load_word.
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_and_one; lia].
      dostep_nary 1. apply r_simple. apply rs_br_if_false.
      rewrite Hmod2. apply N_to_i32_zero.
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 2. apply r_simple.
      eapply rs_binop_success; [reflexivity | apply app_binop_add_one].
      dostep_nary 1.
      { eapply r_local_set with
          (v := N_to_value (n + 1))
          (f' := Build_frame [:: N_to_value addr; N_to_value (n + 1)] inst)
          (vd := N_to_value 0); [reflexivity | cbn; reflexivity | reflexivity]. }
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 1.
      { eapply r_load_success; [exact Hmem |].
        rewrite N_of_uint_N_to_i32; [|assumption].
        change (tnum_length T_i32) with 4%N.
        rewrite load_word_offset. exact Hload. }
      rewrite Htl'.
      dostep_nary 1.
      { eapply r_local_set with
          (v := N_to_value tail)
          (f' := Build_frame [:: N_to_value tail; N_to_value (n + 1)] inst)
          (vd := N_to_value 0); [reflexivity | cbn; reflexivity | reflexivity]. }
      apply rt_refl. }
    eapply rt_trans. { apply reduce_trans_label0. apply br_loop_continue. }
    apply IH.
Qed.

(** [$list_len] returns the length of the spine, allocating nothing. *)
Lemma list_len_spec : forall hs sr inst fr gmp m l bs,
  adapter_inv sr inst gmp ->
  native_at sr inst fidx_list_len (Tf [T_num T_i32] [T_num T_i32]) list_len_func ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  repr_bytes m gmp l bs ->
  reduce_trans
    (hs, sr, fr, [AI_basic (i32c l); AI_basic (BI_call fidx_list_len)])
    (hs, sr, fr, [AI_basic (i32c (N.of_nat (List.length bs)))]).
Proof.
  intros hs sr inst fr gmp m l bs Hinv Hnat Hmem Hfinst Hrepr.
  destruct Hnat as [a [Hidx Hfunc]].
  have Hwrap := ai_no_wrap _ _ _ Hinv.
  (* resolve the call *)
  eapply rt_trans with (y := (hs, sr, fr, [:: AI_basic (i32c l); AI_invoke a])).
  { apply rt_step.
    eapply r_eliml with (lconst := [:: AI_basic (i32c l)]); [reflexivity|].
    apply r_call. rewrite Hfinst. exact Hidx. }
  (* enter the function: one argument, one zero-initialised local *)
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_frame 1 (Build_frame [:: N_to_value l; N_to_value 0] inst)
          [:: AI_label 1 [::] (to_e_list list_len_body)]])).
  { apply rt_step.
    eapply r_invoke_native with
      (cl := FC_func_native (Tf [:: T_num T_i32] [:: T_num T_i32]) inst list_len_func)
      (ts1 := [:: T_num T_i32]) (ts2 := [:: T_num T_i32])
      (code := list_len_func) (x := ty_i32_i32)
      (ts := [:: T_num T_i32]) (es := list_len_body)
      (ves := [:: AI_basic (i32c l)]) (vs := [:: N_to_value l])
      (n := 1) (m := 1) (k := 1) (defaults := [:: N_to_value 0]) (inst := inst);
      try reflexivity.
    exact Hfunc. }
  (* enter the block *)
  eapply rt_trans.
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1. apply rt_step.
    change (to_e_list list_len_body)
      with ([:: AI_basic (BI_block (BT_valtype None)
                            [:: BI_loop (BT_valtype None) list_len_loop])]
            ++ [:: AI_basic (BI_local_get 1%N)]).
    apply r_elimr.
    eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
      reflexivity. }
  (* walk the spine *)
  eapply rt_trans.
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    apply (reduce_trans_elimr [:: AI_basic (BI_local_get 1%N)]).
    change ([::] ++ to_e_list [:: BI_loop (BT_valtype None) list_len_loop])
      with [:: AI_basic (BI_loop (BT_valtype None) list_len_loop)].
    eapply list_len_loop_spec; eassumption. }
  (* read the counter back out and return it *)
  eapply rt_trans.
  { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1. apply rt_step.
    apply r_local_get. reflexivity. }
  eapply rt_trans.
  { apply (reduce_trans_frame 1 fr). apply rt_step. apply r_simple.
    apply rs_label_const. reflexivity. }
  apply rt_step. apply r_simple.
  eapply rs_local_const; reflexivity.
Qed.

(** [$lower] flattens the spine into a fresh buffer and leaves its length in
    [glob_buf_len]. *)
Lemma lower_spec : forall hs sr inst fr gmp m l bs,
  adapter_inv sr inst gmp ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  repr_bytes m gmp l bs ->
  exists hs' sr' res,
    reduce_trans
      (hs, sr, fr, [AI_basic (i32c l); AI_basic (BI_call fidx_lower)])
      (hs', sr', fr, [AI_basic (i32c res)]) /\
    ( (exists m' gmp',
          smem sr' inst = Some m' /\
          adapter_inv sr' inst gmp' /\
          (gmp <= gmp')%N /\
          mem_agree_below m m' gmp /\
          sglob_val sr' inst glob_buf_len
            = Some (N_to_value (N.of_nat (List.length bs))) /\
          buffer_at m' res bs /\
          (res + N.of_nat (List.length bs) <= gmp')%N)
      \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).
Proof. Admitted.

(** [$lift_bytes] builds a heap bytestring from a buffer. *)
Lemma lift_bytes_spec : forall hs sr inst fr gmp m p bs,
  adapter_inv sr inst gmp ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  buffer_at m p bs ->
  (p + N.of_nat (List.length bs) <= gmp)%N ->
  exists hs' sr' res,
    reduce_trans
      (hs, sr, fr,
       [AI_basic (i32c p); AI_basic (i32c (N.of_nat (List.length bs)));
        AI_basic (BI_call fidx_lift_bytes)])
      (hs', sr', fr, [AI_basic (i32c res)]) /\
    ( (exists m' gmp',
          smem sr' inst = Some m' /\
          adapter_inv sr' inst gmp' /\
          (gmp <= gmp')%N /\
          mem_agree_below m m' gmp /\
          repr_bytes m' gmp' res bs)
      \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).
Proof. Admitted.

(** *** [$some]

    An 8-byte boxed [Some]: allocate, check the flag, write the tag and the
    payload, return the cell.  The first of the three helpers to be discharged
    and the simplest — it is straight-line, so the only case analysis is the
    one [alloc_spec] hands over.

    Two hypotheses are not in [adapter_inv] and cannot be:

    - the [native_at] obligations, because [$some] calls [$alloc] by index and
      a theorem about indices is a theorem about an instance;
    - [(gmp + 24 < i32mod)], because [ai_no_wrap] only promises room for the
      one cell the *generated* code is about to need, and a helper that
      allocates needs room for what it is about to allocate too.  24 is
      [4 (alignment) + 8 (the cell) + 12 (what [alloc_spec] asks for)]. *)
Lemma some_spec : forall hs sr inst fr gmp m x bs,
  adapter_inv sr inst gmp ->
  native_at sr inst fidx_some (Tf [T_num T_i32] [T_num T_i32]) some_func ->
  native_at sr inst fidx_alloc
    (Tf [T_num T_i32; T_num T_i32] [T_num T_i32]) alloc_func ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  (gmp + 24 < i32mod)%N ->
  repr_bytes m gmp x bs ->
  exists sr' res,
    reduce_trans
      (hs, sr, fr, [AI_basic (i32c x); AI_basic (BI_call fidx_some)])
      (hs, sr', fr, [AI_basic (i32c res)]) /\
    ( (exists m' gmp',
          smem sr' inst = Some m' /\
          adapter_inv sr' inst gmp' /\
          (gmp <= gmp')%N /\
          mem_agree_below m m' gmp /\
          repr_option_bytes m' gmp' res (Some bs))
      \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).
Proof.
  intros hs sr inst fr gmp m x bs Hinv Hsome Halloc Hmem Hfinst Hroom Hrepr.
  destruct Hsome as [fa [Hidx Hfunc]].
  have Hdist := ai_glob_distinct _ _ _ Hinv.
  (* entering [$some]: one argument, one zero-initialised local *)
  assert (Henter : reduce_trans
    (hs, sr, fr, [:: AI_basic (i32c x); AI_basic (BI_call fidx_some)])
    (hs, sr, fr,
     [:: AI_frame 1 (Build_frame [:: N_to_value x; N_to_value 0] inst)
           [:: AI_label 1 [::]
                 (to_e_list some_alloc ++ to_e_list some_tail)]])).
  { eapply rt_trans with (y := (hs, sr, fr, [:: AI_basic (i32c x); AI_invoke fa])).
    { apply rt_step.
      eapply r_eliml with (lconst := [:: AI_basic (i32c x)]); [reflexivity|].
      apply r_call. rewrite Hfinst. exact Hidx. }
    apply rt_step.
    eapply r_invoke_native with
      (cl := FC_func_native (Tf [:: T_num T_i32] [:: T_num T_i32]) inst some_func)
      (ts1 := [:: T_num T_i32]) (ts2 := [:: T_num T_i32])
      (code := some_func) (x := ty_i32_i32)
      (ts := [:: T_num T_i32]) (es := some_body)
      (ves := [:: AI_basic (i32c x)]) (vs := [:: N_to_value x])
      (n := 1) (m := 1) (k := 1) (defaults := [:: N_to_value 0]) (inst := inst);
      try reflexivity.
    exact Hfunc. }
  (* the allocation: 8 bytes, aligned to 4 *)
  have Hainv := adapter_inv_alloc _ _ _ Hinv.
  destruct (alloc_spec hs sr inst (Build_frame [:: N_to_value x; N_to_value 0] inst)
                       gmp 8 4 Hainv Halloc
                       (ltac:(auto)) (ltac:(lia)) (ltac:(reflexivity)))
    as [sr1 [res [Hred Hcase]]].
  (* ... which leaves the pointer on the stack, whichever way it went *)
  assert (Hafter : reduce_trans
    (hs, sr, fr, [:: AI_basic (i32c x); AI_basic (BI_call fidx_some)])
    (hs, sr1, fr,
     [:: AI_frame 1 (Build_frame [:: N_to_value x; N_to_value 0] inst)
           [:: AI_label 1 [::]
                 ([:: AI_basic (i32c res)] ++ to_e_list some_tail)]])).
  { eapply rt_trans; [exact Henter|].
    apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
    apply (reduce_trans_elimr (to_e_list some_tail)). exact Hred. }
  clear Henter Hred.
  destruct Hcase as [[mi [mi' [Hres [Hmi [Hmi' [Hinv1 Hagree1]]]]]] | Hoom1].
  - (* the allocation succeeded *)
    rewrite Hmem in Hmi. injection Hmi as <-. subst res.
    have [[Hplo Hphi] [kk Hkk]] := alloc_ptr_bounds gmp 4 (ltac:(lia)).
    set (p := alloc_ptr gmp 4) in *.
    assert (Hplt : (p < i32mod)%N) by lia.
    have Hgmp1 := aai_gmp _ _ _ Hinv1.
    have Hoom1 := aai_oom _ _ _ Hinv1.
    have [m1 [Hm1 Hm1len]] := aai_mem _ _ _ Hinv1.
    rewrite Hmi' in Hm1. injection Hm1 as <-.
    (* the two stores *)
    destruct (store_i32_exists mi' (p + 0)%N (VAL_int32 (N_to_i32 0)) (ltac:(lia)))
      as [m2 Hst1].
    (* [smem_store_spec] wants the address as [base + static offset], the heap
       lemmas want it added up; [p + 0] and [p] are equal but not convertible *)
    assert (Hst1' : store_i32 mi' p (VAL_int32 (N_to_i32 0)) = Some m2).
    { replace p with (p + 0)%N by lia. exact Hst1. }
    destruct (smem_store_spec sr1 inst p 0%N (VAL_int32 (N_to_i32 0)) mi' m2
                Hmi' Hst1) as [sr2 [Hsm1 [Hmem2 [Hglob2 _]]]].
    assert (Hlen2 : operations.mem_length m2 = operations.mem_length mi')
      by (eapply store_i32_length; exact Hst1).
    destruct (store_i32_exists m2 (p + 4)%N (VAL_int32 (N_to_i32 x)) (ltac:(lia)))
      as [m3 Hst2].
    destruct (smem_store_spec sr2 inst p 4%N (VAL_int32 (N_to_i32 x)) m2 m3
                Hmem2 Hst2) as [sr3 [Hsm2 [Hmem3 [Hglob3 _]]]].
    assert (Hlen3 : operations.mem_length m3 = operations.mem_length m2)
      by (eapply store_i32_length; exact Hst2).
    exists sr3, p. split.
    { eapply rt_trans; [exact Hafter|].
      eapply rt_trans with (y := (hs, sr3, fr,
        [:: AI_frame 1 (Build_frame [:: N_to_value x; N_to_value p] inst)
              [:: AI_label 1 [::] [:: AI_basic (i32c p)]]])).
      { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
        cbn. unfold i32c, store_word.
        dostep_nary 1.
        { eapply r_local_set with
            (v := N_to_value p)
            (f' := Build_frame [:: N_to_value x; N_to_value p] inst)
            (vd := N_to_value 0); [reflexivity | cbn; reflexivity | reflexivity]. }
        dostep_nary 0. apply r_global_get. exact Hoom1.
        dostep_nary 1. apply r_simple. apply rs_if_false. apply N_to_i32_zero.
        dostep_nary 0.
        eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
          reflexivity.
        dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
        (* cell[0] := 0 *)
        dostep_nary 0. apply r_local_get. reflexivity.
        dostep_nary 2.
        { eapply r_store_success. cbn [f_inst memarg_offset].
          rewrite (N_of_uint_N_to_i32 _ Hplt). exact Hsm1. }
        (* cell[4] := payload *)
        dostep_nary 0. apply r_local_get. reflexivity.
        dostep_nary_eliml 0 1. apply r_local_get. reflexivity.
        dostep_nary 2.
        { eapply r_store_success. cbn [f_inst memarg_offset].
          rewrite (N_of_uint_N_to_i32 _ Hplt). exact Hsm2. }
        (* [separate_instr] has left the frame's locals as a [++]; put them
           back before the last step, whose target frame is concrete *)
        cbn. apply rt_step.
        eapply r_local_get with (v := N_to_value p). reflexivity. }
      eapply rt_trans with (y := (hs, sr3, fr,
        [:: AI_frame 1 (Build_frame [:: N_to_value x; N_to_value p] inst)
              [:: AI_basic (i32c p)]])).
      { apply (reduce_trans_frame 1 fr). apply rt_step. apply r_simple.
        apply rs_label_const. reflexivity. }
      apply rt_step. apply r_simple. eapply rs_local_const; reflexivity. }
    (* what the caller is owed *)
    left. exists m3, (p + 8)%N.
    assert (Hframe : mem_agree_below m m3 gmp).
    { eapply mem_agree_below_trans; [exact Hagree1|].
      eapply mem_agree_below_trans.
      - eapply store_i32_frame; [exact Hst1 | lia].
      - eapply store_i32_frame; [exact Hst2 | lia]. }
    split; [exact Hmem3|].
    split.
    { constructor.
      - rewrite (sglob_val_globals sr1 sr3 inst glob_mem_ptr
                   (ltac:(rewrite Hglob3 Hglob2; reflexivity))).
        exact Hgmp1.
      - exists (2 * kk + 4)%N. lia.
      - rewrite (sglob_val_globals sr1 sr3 inst glob_out_of_mem
                   (ltac:(rewrite Hglob3 Hglob2; reflexivity))).
        exact Hoom1.
      - exists m3. split; [exact Hmem3 | lia].
      - lia.
      - exact Hdist. }
    split; [lia|].
    split; [exact Hframe|].
    eapply repr_some with (payload := x).
    + lia.
    + exists (2 * kk)%N. lia.
    + (* the tag survives the store that follows it *)
      assert (Hdisj : load_i32 m3 p = load_i32 m2 p).
      { eapply load_i32_store_i32_disjoint; [exact Hst2 | lia]. }
      rewrite Hdisj. eapply load_i32_store_i32_same; exact Hst1'.
    + eapply load_i32_store_i32_same; exact Hst2.
    + eapply repr_bytes_gmp_mono; [| eapply repr_bytes_frame; [exact Hframe | exact Hrepr]].
      lia.
  - (* out of memory: the flag is set and [$some] answers [None] *)
    exists sr1, 1%N. split; [| right; exact Hoom1].
    eapply rt_trans; [exact Hafter|].
    eapply rt_trans with (y := (hs, sr1, fr,
      [:: AI_frame 1 (Build_frame [:: N_to_value x; N_to_value res] inst)
            [:: AI_label 1 [::]
                  ([:: AI_label 0 [::] (to_e_list [:: i32c 1; BI_return])]
                   ++ to_e_list some_stores)]])).
    { apply (reduce_trans_frame 1 fr). apply reduce_trans_label1.
      cbn. unfold i32c.
      dostep_nary 1.
      { eapply r_local_set with
          (v := N_to_value res)
          (f' := Build_frame [:: N_to_value x; N_to_value res] inst)
          (vd := N_to_value 0); [reflexivity | cbn; reflexivity | reflexivity]. }
      dostep_nary 0. apply r_global_get. exact Hoom1.
      dostep_nary 1. apply r_simple. apply rs_if_true. apply N_to_i32_one_neq_zero.
      dostep_nary 0.
      eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
        reflexivity.
      apply rt_refl. }
    apply rt_step. apply r_simple.
    eapply rs_return with
      (n := 1) (vs := [:: AI_basic (i32c 1)])
      (lh := LH_rec [::] 1 [::]
               (LH_rec [::] 0 [::] (LH_base [::] [::]) (to_e_list some_stores)) [::]);
      reflexivity.
Qed.

(** ** The adapter obligation

    The theorem the backend's [Eprim] case will consume.  Note the three
    conclusions in the successful disjunct, in decreasing order of how
    surprising they would be if false:

    - [repr_option_bytes m' gmp' v (echo_impl prov bs)]: the published result
      represents what the provider answered.  This is the functional part.
    - [mem_agree_below m m' gmp]: the adapter wrote nothing the generated code
      can observe.  This is what makes the primitive composable at all.
    - [adapter_inv sr' inst gmp']: generated code may resume. *)
Theorem ffi_echo_correct : forall prov hecho hs sr inst fr gmp m arg bs force,
  adapter_inv sr inst gmp ->
  adapter_instance sr inst hecho ->
  echo_provider_spec prov hecho inst ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  repr_bytes m gmp arg bs ->
  exists hs' sr' m' gmp' v,
    reduce_trans
      (hs, sr, fr,
       [AI_basic (i32c force); AI_basic (i32c arg);
        AI_basic (BI_call fidx_export)])
      (hs', sr', fr, []) /\
    smem sr' inst = Some m' /\
    sglob_val sr' inst glob_result = Some (N_to_value v) /\
    ( (adapter_inv sr' inst gmp' /\
       (gmp <= gmp')%N /\
       mem_agree_below m m' gmp /\
       repr_option_bytes m' gmp' v (echo_impl prov bs))
    \/ (sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) /\
        v = unboxed 0) ).
Proof. Admitted.

End Correctness.
