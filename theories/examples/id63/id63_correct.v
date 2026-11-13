(** * The id63 adapter implements its WIT contract

    The theorem this slice exists to make: the adapter module that
    [componentize-formal generate] emitted for

      export id63: func(val: u64) -> result<u64, error>

    ([id63_adapter.v]) writes the canonical-ABI return area of [id63_abi.v]
    correctly, relative to two hypotheses about the CertiRocq-compiled
    backend it is instantiated against ([main_certirocq_spec] and
    [call_target_spec] below) — the id63 instance of the *primitive export
    contract*.

    Unlike [echo_correct.v] there are no loops: the export is straight-line
    with three calls out, so the proof is one pass of the stepping toolkit
    over [export_body], case-split at the three [if]s.

    ** What is hypothesised

    The backend is another core module; this file never sees its code.  What
    it sees is what [core_instantiate.v] delivers: the adapter's imports are
    the backend's exports, at indices fixed by the adapter's own import
    order.  The two behavioural hypotheses say what calling into the backend
    does: [$main_function] leaves the export's closure in [glob_result], and
    calling that closure on a boxed [u63] leaves a boxed result in
    [glob_result] — each with the framing and invariant clauses that CertiRocq's
    correctness theorem provides.

    ** The arms

    - [val >= 2^63]: rejected before the program runs; [err OutOfRange].
    - the backend signals [glob_out_of_mem], before or after the closure
      call: [err OutOfMemory].
    - otherwise: [ok (f val)], where [f] is the function the backend
      computes.

    The one path not covered is the [room_check] trap: a memory too small to
    hold the 24-byte scratch area has no return area to report into, so the
    theorem takes the room as a precondition instead. *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics memory opsem host properties.

From Stdlib Require Import ZArith BinNat NArith.Nnat List Lia
  Relations.Relations Relations.Relation_Operators.

From WasmGlue Require Import wasm_steps wasm_mem certirocq_heap certirocq_inv
  certirocq_adapter.
From wasmcomponents Require Import id63_abi id63_adapter.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.

(** The i32/i64 arithmetic this proof needs ([app_binop_align8],
    [app_relop_lt0_s64_false], [app_relop_lt0_s64_true]) and the
    byte-level store/load toolkit ([store_u8_load_01],
    [load_i32_store_i64_disjoint], ...) live in
    [WasmGlue.certirocq_adapter]: nothing in them mentions a WIT type. *)

Section Correctness.

Context `{ho : host}.

(** ** Function types

    What the WAT declares; [adapter_instance] below pins each body to its
    index at these types. *)
Definition ty_ok      : function_type := Tf [:: T_num T_i32; T_num T_i64] [::].
Definition ty_err     : function_type := Tf [:: T_num T_i32; T_num T_i32] [::].
Definition ty_export  : function_type := Tf [:: T_num T_i64] [:: T_num T_i32].
Definition ty_closure : function_type := Tf [:: T_num T_i32; T_num T_i32] [::].

(** ** [$ok]

    Writes the success arm of the return area: discriminant 0, payload at
    +8.  Everything else in the store survives, which is all the caller
    needs to keep its own invariants. *)
Lemma ok_spec : forall L hs sr inst fr m ret (w : i64),
  native_at sr inst (ly_ok L) ty_ok (ok_func L) ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  exists sr' m',
    reduce_trans
      (hs, sr, fr,
       [:: AI_basic (i32c ret); AI_basic (BI_const_num (VAL_int64 w));
           AI_basic (BI_call (ly_ok L))])
      (hs, sr', fr, [::]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    load_u8 m' (ret + result_u64_disc_off)%N = Some disc_ok /\
    load_i64 m' (ret + result_u64_payload_off)%N = Some (VAL_int64 w).
Proof.
  intros L hs sr inst fr m ret w Hnat Hmem Hfinst Hroom Hret.
  destruct Hnat as [fa [Hidx Hfunc]].
  (* the two stores, in memory terms first *)
  destruct (store_u8_exists m ret (N_to_i32 0) (ltac:(lia))) as [m1 Hst1].
  destruct (smem_store_packed_u8_spec sr inst ret 0%N (N_to_i32 0) m m1 Hmem
              (ltac:(rewrite N.add_0_r; exact Hst1)))
    as [sr1 [Hsm1 [Hmem1 [Hg1 [Hf1 Ht1]]]]].
  assert (Hlen1 : operations.mem_length m1 = operations.mem_length m)
    by (eapply store_u8_length; exact Hst1).
  destruct (store_i64_exists m1 (ret + 8)%N (VAL_int64 w) (ltac:(lia))) as [m2 Hst2].
  destruct (smem_store_i64_spec sr1 inst ret 8%N (VAL_int64 w) m1 m2 Hmem1 Hst2)
    as [sr2 [Hsm2 [Hmem2 [Hg2 [Hf2 Ht2]]]]].
  assert (Hlen2 : operations.mem_length m2 = operations.mem_length m1)
    by (eapply store_i64_length; exact Hst2).
  exists sr2, m2. split.
  { (* the run *)
    eapply rt_trans with (y := (hs, sr, fr,
      [:: AI_basic (i32c ret); AI_basic (BI_const_num (VAL_int64 w));
          AI_invoke fa])).
    { apply rt_step.
      eapply r_eliml with (lconst := [:: AI_basic (i32c ret);
                                         AI_basic (BI_const_num (VAL_int64 w))]);
        [reflexivity|].
      apply r_call. rewrite Hfinst. exact Hidx. }
    eapply rt_trans with (y := (hs, sr, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret; VAL_num (VAL_int64 w)] inst)
            [:: AI_label 0 [::] (to_e_list ok_body)]])).
    { apply rt_step.
      eapply r_invoke_native with
        (cl := FC_func_native ty_ok inst (ok_func L))
        (ts1 := [:: T_num T_i32; T_num T_i64]) (ts2 := [::])
        (code := ok_func L) (x := ly_ty_ok L)
        (ts := [::]) (es := ok_body)
        (ves := [:: AI_basic (i32c ret); AI_basic (BI_const_num (VAL_int64 w))])
        (vs := [:: N_to_value ret; VAL_num (VAL_int64 w)])
        (n := 2) (m := 0) (k := 0) (defaults := [::]) (inst := inst);
        try reflexivity.
      exact Hfunc. }
    eapply rt_trans with (y := (hs, sr2, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret; VAL_num (VAL_int64 w)] inst)
            [:: AI_label 0 [::] [::]]])).
    { apply (reduce_trans_frame 0 fr). apply reduce_trans_label0.
      cbn. unfold i32c, store_byte, store_i64_at.
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 2.
      { eapply r_store_packed_success; [| reflexivity].
        cbn [f_inst memarg_offset].
        rewrite (N_of_uint_N_to_i32 _ Hret). exact Hsm1. }
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary_eliml 0 1. apply r_local_get. reflexivity.
      dostep_nary' 2.
      { eapply r_store_success. cbn [f_inst memarg_offset].
        rewrite (N_of_uint_N_to_i32 _ Hret). exact Hsm2. }
      apply rt_refl. }
    eapply rt_trans with (y := (hs, sr2, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret; VAL_num (VAL_int64 w)] inst)
            [::]])).
    { apply (reduce_trans_frame 0 fr). apply rt_step. apply r_simple.
      apply rs_label_const. reflexivity. }
    apply rt_step. apply r_simple. eapply rs_local_const; reflexivity. }
  split; [exact Hmem2|].
  split; [rewrite Hg2 Hg1; reflexivity|].
  split; [rewrite Hf2 Hf1; reflexivity|].
  split; [rewrite Ht2 Ht1; reflexivity|].
  split; [rewrite Hlen2 Hlen1; reflexivity|].
  split.
  { eapply mem_agree_below_trans.
    - eapply store_frame_gen; [exact Hst1 | lia].
    - eapply store_i64_frame; [exact Hst2 | lia]. }
  split.
  { (* the discriminant survives the payload store *)
    unfold result_u64_disc_off. rewrite N.add_0_r.
    rewrite (load_u8_store_i64_disjoint m1 m2 (ret + 8)%N ret _ Hst2 (ltac:(lia))).
    eapply store_u8_load_01; [left; reflexivity | exact Hst1]. }
  unfold result_u64_payload_off.
  eapply load_i64_store_i64_same; exact Hst2.
Qed.

(** ** [$err]

    The failure arm: discriminant 1, and the error enum's own discriminant
    where the payload would be. *)
Lemma err_spec : forall L hs sr inst fr m ret (e : abi_error),
  native_at sr inst (ly_err L) ty_err (err_func L) ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  exists sr' m',
    reduce_trans
      (hs, sr, fr,
       [:: AI_basic (i32c ret); AI_basic (i32c (error_code e));
           AI_basic (BI_call (ly_err L))])
      (hs, sr', fr, [::]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    load_u8 m' (ret + result_u64_disc_off)%N = Some disc_err /\
    load_u8 m' (ret + result_u64_payload_off)%N = Some (error_code e).
Proof.
  intros L hs sr inst fr m ret e Hnat Hmem Hfinst Hroom Hret.
  destruct Hnat as [fa [Hidx Hfunc]].
  destruct (store_u8_exists m ret (N_to_i32 1) (ltac:(lia))) as [m1 Hst1].
  destruct (smem_store_packed_u8_spec sr inst ret 0%N (N_to_i32 1) m m1 Hmem
              (ltac:(rewrite N.add_0_r; exact Hst1)))
    as [sr1 [Hsm1 [Hmem1 [Hg1 [Hf1 Ht1]]]]].
  assert (Hlen1 : operations.mem_length m1 = operations.mem_length m)
    by (eapply store_u8_length; exact Hst1).
  destruct (store_u8_exists m1 (ret + 8)%N (N_to_i32 (error_code e)) (ltac:(lia)))
    as [m2 Hst2].
  destruct (smem_store_packed_u8_spec sr1 inst ret 8%N (N_to_i32 (error_code e))
              m1 m2 Hmem1 Hst2)
    as [sr2 [Hsm2 [Hmem2 [Hg2 [Hf2 Ht2]]]]].
  assert (Hlen2 : operations.mem_length m2 = operations.mem_length m1)
    by (eapply store_u8_length; exact Hst2).
  exists sr2, m2. split.
  { eapply rt_trans with (y := (hs, sr, fr,
      [:: AI_basic (i32c ret); AI_basic (i32c (error_code e)); AI_invoke fa])).
    { apply rt_step.
      eapply r_eliml with (lconst := [:: AI_basic (i32c ret);
                                         AI_basic (i32c (error_code e))]);
        [reflexivity|].
      apply r_call. rewrite Hfinst. exact Hidx. }
    eapply rt_trans with (y := (hs, sr, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret;
                                      N_to_value (error_code e)] inst)
            [:: AI_label 0 [::] (to_e_list err_body)]])).
    { apply rt_step.
      eapply r_invoke_native with
        (cl := FC_func_native ty_err inst (err_func L))
        (ts1 := [:: T_num T_i32; T_num T_i32]) (ts2 := [::])
        (code := err_func L) (x := ly_ty_err L)
        (ts := [::]) (es := err_body)
        (ves := [:: AI_basic (i32c ret); AI_basic (i32c (error_code e))])
        (vs := [:: N_to_value ret; N_to_value (error_code e)])
        (n := 2) (m := 0) (k := 0) (defaults := [::]) (inst := inst);
        try reflexivity.
      exact Hfunc. }
    eapply rt_trans with (y := (hs, sr2, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret;
                                      N_to_value (error_code e)] inst)
            [:: AI_label 0 [::] [::]]])).
    { apply (reduce_trans_frame 0 fr). apply reduce_trans_label0.
      cbn. unfold i32c, store_byte.
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary 2.
      { eapply r_store_packed_success; [| reflexivity].
        cbn [f_inst memarg_offset].
        rewrite (N_of_uint_N_to_i32 _ Hret). exact Hsm1. }
      dostep_nary 0. apply r_local_get. reflexivity.
      dostep_nary_eliml 0 1. apply r_local_get. reflexivity.
      dostep_nary' 2.
      { eapply r_store_packed_success; [| reflexivity].
        cbn [f_inst memarg_offset].
        rewrite (N_of_uint_N_to_i32 _ Hret). exact Hsm2. }
      apply rt_refl. }
    eapply rt_trans with (y := (hs, sr2, fr,
      [:: AI_frame 0 (Build_frame [:: N_to_value ret;
                                      N_to_value (error_code e)] inst)
            [::]])).
    { apply (reduce_trans_frame 0 fr). apply rt_step. apply r_simple.
      apply rs_label_const. reflexivity. }
    apply rt_step. apply r_simple. eapply rs_local_const; reflexivity. }
  split; [exact Hmem2|].
  split; [rewrite Hg2 Hg1; reflexivity|].
  split; [rewrite Hf2 Hf1; reflexivity|].
  split; [rewrite Ht2 Ht1; reflexivity|].
  split; [rewrite Hlen2 Hlen1; reflexivity|].
  split.
  { eapply mem_agree_below_trans.
    - eapply store_frame_gen; [exact Hst1 | lia].
    - eapply store_frame_gen; [exact Hst2 | lia]. }
  split.
  { unfold result_u64_disc_off. rewrite N.add_0_r.
    rewrite (load_u8_store_disjoint m1 m2 (ret + 8)%N ret _ _ Hst2
               (ltac:(cbn; lia))).
    eapply store_u8_load_01; [right; reflexivity | exact Hst1]. }
  unfold result_u64_payload_off.
  eapply store_u8_load_01; [| exact Hst2].
  destruct e; [left | right]; reflexivity.
Qed.

(** ** The export's frame

    One [i64] parameter, three [i32] locals: [ret], [arg], [clos]. *)
Definition exp_locs (c : i64) (ret arg clos : N) (inst : moduleinst) : frame :=
  Build_frame [:: VAL_num (VAL_int64 c);
                  N_to_value ret; N_to_value arg; N_to_value clos] inst.

Arguments exp_locs : simpl never.

(** ** What the instance must look like

    The three adapter bodies at their indices, and the closure's type index
    resolving to the closure calling convention — all discharged by
    [core_instantiate.v] once the component's instantiation is in view. *)
Record adapter_instance (L : layout) (fi : funcidx)
       (sr : store_record) (inst : moduleinst) : Prop := {
  adi_export : native_at sr inst fi ty_export (export_func L);
  adi_ok     : native_at sr inst (ly_ok L) ty_ok (ok_func L);
  adi_err    : native_at sr inst (ly_err L) ty_err (err_func L);
  adi_ty_closure : lookup_N (inst_types inst) (ly_ty_closure L) = Some ty_closure
}.

(** ** The backend hypotheses

    What CertiRocq's correctness theorem will eventually discharge, stated
    over this repository's [adapter_inv] fragment.  [f] is the function the
    compiled program denotes on [u63] values.

    [call_target_spec a]: invoking the closure's code at [a] on a boxed
    argument leaves a boxed [f z] in [glob_result] — or signals
    [glob_out_of_mem].  [main_certirocq_spec]: [$main_function] leaves the
    export's closure in [glob_result] — or signals; the closure's two fields
    and its table entry come with it, as does the behaviour of the entry. *)
Definition call_target_spec (inst : moduleinst) (f : Z -> Z)
                            (a : funcaddr) : Prop :=
  forall hs sr fr m gmp env argp z,
    f_inst fr = inst ->
    adapter_inv sr inst gmp ->
    smem sr inst = Some m ->
    repr_prim m gmp argp z ->
    (argp < i32mod)%N ->
    (0 <= z < 2 ^ 63)%Z ->
    exists hs' sr' m',
      reduce_trans
        (hs, sr, fr,
         [:: AI_basic (BI_const_num (VAL_int32 (N_to_i32 env)));
             AI_basic (BI_const_num (VAL_int32 (N_to_i32 argp)));
             AI_invoke a])
        (hs', sr', fr, [::]) /\
      smem sr' inst = Some m' /\
      (operations.mem_length m <= operations.mem_length m')%N /\
      mem_agree_below m m' gmp /\
      s_funcs sr' = s_funcs sr /\
      s_tables sr' = s_tables sr /\
      ( (exists gmp' res,
           adapter_inv sr' inst gmp' /\
           (gmp <= gmp')%N /\
           sglob_val sr' inst glob_result = Some (N_to_value res) /\
           (res < i32mod)%N /\
           repr_prim m' gmp' res (f z))
        \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).

Definition main_certirocq_spec (L : layout) (f : Z -> Z) (inst : moduleinst) : Prop :=
  forall hs sr fr gmp m,
    f_inst fr = inst ->
    adapter_inv sr inst gmp ->
    smem sr inst = Some m ->
    exists hs' sr' m',
      reduce_trans
        (hs, sr, fr, [:: AI_basic (BI_call (ly_main L))])
        (hs', sr', fr, [::]) /\
      smem sr' inst = Some m' /\
      (operations.mem_length m <= operations.mem_length m')%N /\
      mem_agree_below m m' gmp /\
      s_funcs sr' = s_funcs sr /\
      s_tables sr' = s_tables sr /\
      ( (exists gmp' clos tfi env a cl,
           adapter_inv sr' inst gmp' /\
           (gmp <= gmp')%N /\
           sglob_val sr' inst glob_result = Some (N_to_value clos) /\
           (gmp <= clos)%N /\ (clos + 12 <= gmp')%N /\ (clos < i32mod)%N /\
           load_i32 m' (clos + 4)%N = Some (VAL_int32 (N_to_i32 tfi)) /\
           load_i32 m' (clos + 8)%N = Some (VAL_int32 (N_to_i32 env)) /\
           (tfi < i32mod)%N /\
           stab_elem sr' inst tbl_funcs tfi = Some (VAL_ref_func a) /\
           lookup_N (s_funcs sr') a = Some cl /\
           cl_type cl = ty_closure /\
           call_target_spec inst f a)
        \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).

(** ** The failure path

    Both error arms run [fail_with]: answer [err], put the return area's
    address back on the stack, and [return] out through the [if]'s label and
    the function's.  Stated at the whole-frame level so the three call sites
    can hand over directly after dispatching their [if]. *)
Lemma fail_spec : forall L hs sr inst fr0 (c : i64) ret arg clos
                         (e : abi_error) rest m,
  native_at sr inst (ly_err L) ty_err (err_func L) ->
  smem sr inst = Some m ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  exists sr' m',
    reduce_trans
      (hs, sr, fr0,
       [:: AI_frame 1 (exp_locs c ret arg clos inst)
             [:: AI_label 1 [::]
                   ([:: AI_label 0 [::]
                          (to_e_list (fail_case L (error_code e)))]
                    ++ rest)]])
      (hs, sr', fr0, [:: AI_basic (i32c ret)]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    canon_result_u64 m' ret (inr e).
Proof.
  intros L hs sr inst fr0 c ret arg clos e rest m Herr Hmem Hroom Hret.
  destruct (err_spec L hs sr inst (exp_locs c ret arg clos inst) m ret e
              Herr Hmem (ltac:(reflexivity)) Hroom Hret)
    as [sr' [m' [Hred [Hmem' [Hg' [Hf' [Ht' [Hlen' [Hagree [Hd8 Hp8]]]]]]]]]].
  exists sr', m'. split.
  { eapply rt_trans with (y := (hs, sr', fr0,
      [:: AI_frame 1 (exp_locs c ret arg clos inst)
            [:: AI_label 1 [::]
                  ([:: AI_label 0 [::]
                         ([:: AI_basic (i32c ret)] ++ [:: AI_basic BI_return])]
                   ++ rest)]])).
    { apply (reduce_trans_frame 1 fr0). apply reduce_trans_label1.
      apply (reduce_trans_elimr rest). apply reduce_trans_label0.
      cbn. unfold i32c.
      dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
      eapply rt_trans with (y := (hs, sr', exp_locs c ret arg clos inst,
        ([:: AI_basic (BI_local_get 1%N)] ++ [:: AI_basic BI_return]))).
      (* [exact], not [apply]: with [Hred] supplied the two instruction lists
         are only convertible, not syntactically equal — [separate_instr] has
         re-associated the goal's. *)
      { exact (reduce_trans_elimr
                 ([:: AI_basic (BI_local_get 1%N)] ++ [:: AI_basic BI_return])
                 _ _ _ _ _ _ _ _ Hred). }
      have Hlg : reduce_trans
        (hs, sr', exp_locs c ret arg clos inst,
         [:: AI_basic (BI_local_get 1%N)])
        (hs, sr', exp_locs c ret arg clos inst,
         [:: $V N_to_value ret]).
      { apply rt_step. apply r_local_get. unfold exp_locs. reflexivity. }
      exact (reduce_trans_elimr
               [:: AI_basic BI_return] _ _ _ _ _ _ _ _ Hlg). }
    apply rt_step. apply r_simple.
    eapply rs_return with
      (n := 1) (vs := [:: AI_basic (i32c ret)])
      (lh := LH_rec [::] 1 [::]
               (LH_rec [::] 0 [::] (LH_base [::] [::]) rest) [::]);
      reflexivity. }
  split; [exact Hmem'|]. split; [exact Hg'|]. split; [exact Hf'|].
  split; [exact Ht'|]. split; [exact Hlen'|]. split; [exact Hagree|].
  split; [exact Hd8 | exact Hp8].
Qed.

(** ** The prologue and the room check

    Straight-line from entry to the end of [room_check], on the arm where
    the memory is big enough: the scratch area is carved out, the three
    pointers are in their locals, and the store has only moved
    [glob_mem_ptr]. *)
Lemma prologue_room_spec : forall hs sr inst (c : i64) gmp m rest,
  adapter_inv sr inst gmp ->
  smem sr inst = Some m ->
  (alloc_ptr gmp 8 + 24 <= mem_size m * page_bytes)%N ->
  (mem_size m * page_bytes < i32mod)%N ->
  exists sr',
    reduce_trans
      (hs, sr, exp_locs c 0 0 0 inst,
       to_e_list (prologue ++ room_check) ++ rest)
      (hs, sr',
       exp_locs c (alloc_ptr gmp 8) (alloc_ptr gmp 8 + 16) 0 inst,
       rest) /\
    adapter_inv sr' inst (alloc_ptr gmp 8 + 24) /\
    scratch_ok 16 24 (alloc_ptr gmp 8) (alloc_ptr gmp 8 + 16)
               (alloc_ptr gmp 8 + 24) /\
    smem sr' inst = Some m /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr.
Proof.
  intros hs sr inst c gmp m rest Hinv Hmem Hroom Hszmod.
  have Hgmp := ai_gmp _ _ _ Hinv.
  have [k Heven] := ai_gmp_even _ _ _ Hinv.
  have Hoom := ai_oom _ _ _ Hinv.
  have Hnw := ai_no_wrap _ _ _ Hinv.
  have Hdist := ai_glob_distinct _ _ _ Hinv.
  have [[Hplo Hphi] [k8 Hk8]] := alloc_ptr_bounds gmp 8 (ltac:(lia)).
  set (R := alloc_ptr gmp 8) in *.
  (* the memory covers [mem_size * page] bytes *)
  assert (Hcov : (mem_size m * page_bytes <= operations.mem_length m)%N).
  { unfold mem_size, page_bytes. change 65536%N with page_size.
    rewrite N.mul_comm. apply N.Div0.mul_div_le. }
  assert (HRmod : (R + 24 < i32mod)%N) by lia.
  (* the two global.sets *)
  have [sra Hupd1] := supdate_glob_exists sr inst glob_mem_ptr
                        (N_to_value R) _ Hgmp.
  assert (Hgmpa : sglob_val sra inst glob_mem_ptr = Some (N_to_value R))
    by (apply (supdate_glob_same _ _ _ _ _ Hupd1)).
  have [srb Hupd2] := supdate_glob_exists sra inst glob_mem_ptr
                        (N_to_value (R + 24)) _ Hgmpa.
  assert (Hgmpb : sglob_val srb inst glob_mem_ptr = Some (N_to_value (R + 24)))
    by (apply (supdate_glob_same _ _ _ _ _ Hupd2)).
  assert (Hmema : smem sra inst = Some m)
    by (rewrite (supdate_glob_mem _ _ _ _ _ Hupd1); exact Hmem).
  assert (Hmemb : smem srb inst = Some m)
    by (rewrite (supdate_glob_mem _ _ _ _ _ Hupd2); exact Hmema).
  assert (Hooma : sglob_val sra inst glob_out_of_mem = Some (N_to_value 0))
    by (eapply supdate_glob_other; [exact Hdist | exact Hoom | exact Hupd1]).
  (* [sglob_ind] ignores the store, so the index distinctness transports by
     conversion rather than by rewriting. *)
  assert (Hdista : sglob_ind sra inst glob_mem_ptr
                  <> sglob_ind sra inst glob_out_of_mem) by exact Hdist.
  assert (Hoomb : sglob_val srb inst glob_out_of_mem = Some (N_to_value 0))
    by (eapply supdate_glob_other; [exact Hdista | exact Hooma | exact Hupd2]).
  (* [room_check] is a 32-bit comparison, so a memory whose size in bytes it
     can represent has at most 65535 pages — which leaves far more than the
     12 bytes [adapter_inv] asks for above [glob_mem_ptr]. *)
  assert (Hslack : (R + 24 + 12 < i32mod)%N).
  { have Hi32 : i32mod = 4294967296%N by (unfold i32mod; vm_compute; reflexivity).
    unfold page_bytes in Hroom, Hszmod. rewrite Hi32 in Hszmod |- *. lia. }
  exists srb. split.
  { cbn. unfold i32c, i32c_neg8, add, and_, mul, gt_u.
    (* mem_ptr := align8 mem_ptr *)
    dostep_nary 0. apply r_global_get. exact Hgmp.
    dostep_nary 2. apply r_simple.
    eapply rs_binop_success; [reflexivity | apply app_binop_add].
    dostep_nary 2. apply r_simple.
    eapply rs_binop_success; [reflexivity | apply app_binop_align8; lia].
    dostep_nary 1. eapply r_global_set with (v := N_to_value R). exact Hupd1.
    (* ret := mem_ptr *)
    dostep_nary 0. apply r_global_get. exact Hgmpa.
    dostep_nary 1.
    { eapply r_local_set with
        (v := N_to_value R)
        (f' := exp_locs c R 0 0 inst)
        (vd := N_to_value 0);
        [reflexivity | unfold exp_locs; cbn; reflexivity
         | unfold exp_locs; cbn; reflexivity]. }
    (* arg := ret + 16 *)
    dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary 2. apply r_simple.
    eapply rs_binop_success; [reflexivity | apply app_binop_add].
    dostep_nary 1.
    { eapply r_local_set with
        (v := N_to_value (R + 16))
        (f' := exp_locs c R (R + 16) 0 inst)
        (vd := N_to_value 0);
        [reflexivity | unfold exp_locs; cbn; reflexivity
         | unfold exp_locs; cbn; reflexivity]. }
    (* mem_ptr := ret + 24 *)
    dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary 2. apply r_simple.
    eapply rs_binop_success; [reflexivity | apply app_binop_add].
    dostep_nary 1. eapply r_global_set with (v := N_to_value (R + 24)). exact Hupd2.
    (* room check: mem_ptr >u size * page ? *)
    dostep_nary 0. apply r_global_get. exact Hgmpb.
    dostep_nary_eliml 0 1. eapply r_memory_size; [exact Hmemb | reflexivity].
    dostep_nary_eliml 2 1. apply r_simple.
    eapply rs_binop_success; [reflexivity | apply app_binop_mul].
    dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
    (* the comparison's value stays a side goal, as in [echo_correct.v] *)
    dostep_nary 1. apply r_simple. cbn. apply rs_if_false_bool.
    { (* [cbn] has already unfolded [app_relop]; fold it back by conversion *)
      change (app_relop (Relop_i (ROI_gt SX_U))
                (VAL_int32 (N_to_i32 (R + 24)))
                (VAL_int32 (N_to_i32 (mem_size m * page_bytes))) = false).
      rewrite (app_relop_gt_u _ _ HRmod Hszmod). apply N.ltb_ge. lia. }
    dostep_nary 0.
    eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
      reflexivity.
    dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
    apply rt_refl. }
  split.
  { constructor.
    - exact Hgmpb.
    - exists (4 * k8 + 12)%N. lia.
    - exact Hoomb.
    - exists m. split; [exact Hmemb | lia].
    - exact Hslack.
    - exact Hdist. }
  split.
  { constructor; [reflexivity | lia | lia | exists (4 * k8 + 8)%N; lia | lia]. }
  split; [exact Hmemb|].
  split.
  { rewrite (supdate_glob_funcs _ _ _ _ _ Hupd2)
            (supdate_glob_funcs _ _ _ _ _ Hupd1). reflexivity. }
  rewrite (supdate_glob_tables _ _ _ _ _ Hupd2)
          (supdate_glob_tables _ _ _ _ _ Hupd1). reflexivity.
Qed.

(** ** Entering and leaving the export

    The body splits along the branches the proof takes; [to_e_list]
    distributes over the pieces by computation. *)
Lemma to_e_list_export_body : forall L,
  to_e_list (export_body L)
  = (to_e_list (prologue ++ room_check)
     ++ to_e_list (range_check L)
     ++ [:: AI_basic (BI_call (ly_main L))]
     ++ to_e_list (oom_check L)
     ++ [:: AI_basic (BI_global_get glob_result);
            AI_basic (BI_local_set 3%N)]
     ++ to_e_list (call_closure L)
     ++ to_e_list (oom_check L)
     ++ to_e_list (epilogue L))%list.
Proof. reflexivity. Qed.

Lemma export_entry : forall L fi hs sr inst fr (c : i64),
  native_at sr inst fi ty_export (export_func L) ->
  f_inst fr = inst ->
  reduce_trans
    (hs, sr, fr,
     [:: AI_basic (BI_const_num (VAL_int64 c)); AI_basic (BI_call fi)])
    (hs, sr, fr,
     [:: AI_frame 1 (exp_locs c 0 0 0 inst)
           [:: AI_label 1 [::] (to_e_list (export_body L))]]).
Proof.
  intros L fi hs sr inst fr c [fa [Hidx Hfunc]] Hfinst.
  eapply rt_trans with (y := (hs, sr, fr,
    [:: AI_basic (BI_const_num (VAL_int64 c)); AI_invoke fa])).
  { apply rt_step.
    eapply r_eliml with (lconst := [:: AI_basic (BI_const_num (VAL_int64 c))]);
      [reflexivity|].
    apply r_call. rewrite Hfinst. exact Hidx. }
  apply rt_step.
  eapply r_invoke_native with
    (cl := FC_func_native ty_export inst (export_func L))
    (ts1 := [:: T_num T_i64]) (ts2 := [:: T_num T_i32])
    (code := export_func L) (x := ly_ty_export L)
    (ts := repeat (T_num T_i32) 3) (es := export_body L)
    (ves := [:: AI_basic (BI_const_num (VAL_int64 c))])
    (vs := [:: VAL_num (VAL_int64 c)])
    (n := 1) (m := 1) (k := 3)
    (defaults := [:: N_to_value 0; N_to_value 0; N_to_value 0])
    (inst := inst);
    try reflexivity.
  exact Hfunc.
Qed.

(** The return area's address is the one value the export leaves behind. *)
Lemma export_exit : forall hs sr inst fr0 (c : i64) ret arg clos,
  reduce_trans
    (hs, sr, fr0,
     [:: AI_frame 1 (exp_locs c ret arg clos inst)
           [:: AI_label 1 [::] [:: AI_basic (i32c ret)]]])
    (hs, sr, fr0, [:: AI_basic (i32c ret)]).
Proof.
  intros hs sr inst fr0 c ret arg clos.
  eapply rt_trans with (y := (hs, sr, fr0,
    [:: AI_frame 1 (exp_locs c ret arg clos inst) [:: AI_basic (i32c ret)]])).
  { apply (reduce_trans_frame 1 fr0). apply rt_step. apply r_simple.
    apply rs_label_const. reflexivity. }
  apply rt_step. apply r_simple. eapply rs_local_const; reflexivity.
Qed.

(** ** The range check

    [i64.lt_s] against zero is the whole test: a [u64] of 2^63 or more reads
    as a negative [i64], and has no [Uint63.int]. *)
Lemma range_check_pass : forall L hs sr inst (z : Z) ret arg clos rest,
  (0 <= z < 2 ^ 63)%Z ->
  reduce_trans
    (hs, sr, exp_locs (Z_to_i64 z) ret arg clos inst,
     to_e_list (range_check L) ++ rest)
    (hs, sr, exp_locs (Z_to_i64 z) ret arg clos inst, rest).
Proof.
  intros L hs sr inst z ret arg clos rest Hz.
  cbn. unfold i64c, lt_s64.
  dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
  dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
  dostep_nary 1. apply r_simple. cbn. apply rs_if_false_bool.
  { change (app_relop (Relop_i (ROI_lt SX_S))
              (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0)) = false).
    apply app_relop_lt0_s64_false. exact Hz. }
  dostep_nary 0.
  eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
    reflexivity.
  dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
  apply rt_refl.
Qed.

Lemma range_check_fail : forall L hs sr inst fr0 (z : Z) ret arg clos rest m,
  native_at sr inst (ly_err L) ty_err (err_func L) ->
  smem sr inst = Some m ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  (2 ^ 63 <= z < 2 ^ 64)%Z ->
  exists sr' m',
    reduce_trans
      (hs, sr, fr0,
       [:: AI_frame 1 (exp_locs (Z_to_i64 z) ret arg clos inst)
             [:: AI_label 1 [::] (to_e_list (range_check L) ++ rest)]])
      (hs, sr', fr0, [:: AI_basic (i32c ret)]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    canon_result_u64 m' ret (inr OutOfRange).
Proof.
  intros L hs sr inst fr0 z ret arg clos rest m Herr Hmem Hroom Hret Hz.
  destruct (fail_spec L hs sr inst fr0 (Z_to_i64 z) ret arg clos
              OutOfRange rest m Herr Hmem Hroom Hret)
    as [sr' [m' [Hred Hrest]]].
  exists sr', m'. split; [| exact Hrest].
  eapply rt_trans; [| exact Hred].
  apply (reduce_trans_frame 1 fr0). apply reduce_trans_label1.
  cbn. unfold i64c, lt_s64.
  dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
  dostep_nary 2. apply r_simple. apply rs_relop. reflexivity.
  dostep_nary 1. apply r_simple. cbn. apply rs_if_true_bool.
  { change (app_relop (Relop_i (ROI_lt SX_S))
              (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0)) = true).
    apply app_relop_lt0_s64_true. exact Hz. }
  dostep_nary 0.
  eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
    reflexivity.
  apply rt_refl.
Qed.

(** ** The out-of-memory check

    Run twice: [glob_result] is only meaningful when the flag is clear. *)
Lemma oom_check_pass : forall L hs sr fr rest,
  sglob_val sr (f_inst fr) glob_out_of_mem = Some (N_to_value 0) ->
  reduce_trans (hs, sr, fr, to_e_list (oom_check L) ++ rest)
               (hs, sr, fr, rest).
Proof. intros L. apply oom_guard_pass. Qed.

Lemma oom_check_fail : forall L hs sr inst fr0 (c : i64) ret arg clos rest m,
  native_at sr inst (ly_err L) ty_err (err_func L) ->
  sglob_val sr inst glob_out_of_mem = Some (N_to_value 1) ->
  smem sr inst = Some m ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  exists sr' m',
    reduce_trans
      (hs, sr, fr0,
       [:: AI_frame 1 (exp_locs c ret arg clos inst)
             [:: AI_label 1 [::] (to_e_list (oom_check L) ++ rest)]])
      (hs, sr', fr0, [:: AI_basic (i32c ret)]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    canon_result_u64 m' ret (inr OutOfMemory).
Proof.
  intros L hs sr inst fr0 c ret arg clos rest m Herr Hoom Hmem Hroom Hret.
  destruct (fail_spec L hs sr inst fr0 c ret arg clos
              OutOfMemory rest m Herr Hmem Hroom Hret)
    as [sr' [m' [Hred Hrest]]].
  exists sr', m'. split; [| exact Hrest].
  eapply rt_trans; [| exact Hred].
  apply (reduce_trans_frame 1 fr0). apply reduce_trans_label1.
  cbn.
  dostep_nary 0. apply r_global_get. unfold exp_locs. exact Hoom.
  dostep_nary 1. apply r_simple. apply rs_if_true_N1.
  dostep_nary 0.
  eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
    reflexivity.
  apply rt_refl.
Qed.

(** ** The epilogue

    [glob_result] points at the payload, not the value, so the answer is read
    out of the heap before [$ok] writes it into the return area. *)
Lemma epilogue_spec : forall L hs sr inst (c : i64) ret arg clos res (w : i64) m,
  native_at sr inst (ly_ok L) ty_ok (ok_func L) ->
  smem sr inst = Some m ->
  sglob_val sr inst glob_result = Some (N_to_value res) ->
  (res < i32mod)%N ->
  load_i64 m res = Some (VAL_int64 w) ->
  (ret + 16 <= operations.mem_length m)%N ->
  (ret < i32mod)%N ->
  exists sr' m',
    reduce_trans
      (hs, sr, exp_locs c ret arg clos inst, to_e_list (epilogue L))
      (hs, sr', exp_locs c ret arg clos inst, [:: AI_basic (i32c ret)]) /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    operations.mem_length m' = operations.mem_length m /\
    mem_agree_below m m' ret /\
    canon_result_u64 m' ret (inl w).
Proof.
  intros L hs sr inst c ret arg clos res w m Hok Hmem Hres Hresmod Hld Hroom Hret.
  (* the payload, as [r_load_success] will read it *)
  unfold load_i64 in Hld.
  destruct (load m res 0%N 8) as [bs|] eqn:Hload; [|discriminate].
  assert (Hbs : wasm_deserialise bs T_i64 = VAL_int64 w) by congruence.
  destruct (ok_spec L hs sr inst (exp_locs c ret arg clos inst) m ret w
              Hok Hmem (ltac:(reflexivity)) Hroom Hret)
    as [sr' [m' [Hred [Hmem' [Hg' [Hf' [Ht' [Hlen' [Hagree [Hd8 Hp8]]]]]]]]]].
  exists sr', m'. split.
  { cbn. unfold i32c, load_i64_at.
    dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary_eliml 0 1. apply r_global_get. unfold exp_locs. exact Hres.
    dostep_nary_eliml 1 1.
    { eapply r_load_success; [unfold exp_locs; exact Hmem |].
      rewrite N_of_uint_N_to_i32; [| exact Hresmod].
      change (tnum_length T_i64) with 8%N. exact Hload. }
    rewrite Hbs.
    eapply rt_trans with (y := (hs, sr', exp_locs c ret arg clos inst,
      [:: AI_basic (BI_local_get 1%N)])).
    { exact (reduce_trans_elimr
               [:: AI_basic (BI_local_get 1%N)] _ _ _ _ _ _ _ _ Hred). }
    apply rt_step. eapply r_local_get with (v := N_to_value ret).
    unfold exp_locs. reflexivity. }
  split; [exact Hmem'|]. split; [exact Hg'|]. split; [exact Hf'|].
  split; [exact Ht'|]. split; [exact Hlen'|]. split; [exact Hagree|].
  split; [exact Hd8 | exact Hp8].
Qed.

(** ** Calling the closure

    The argument is boxed into the adapter's 8-byte scratch cell, the
    closure's [env] and [func_idx] fields are read out of the heap, and the
    call goes through the table at the type the component declared.  The box
    sits above the return area and below the allocation frontier, so neither
    the answer being written nor anything the backend allocated is disturbed
    — which is what [scratch_ok] says, so the disjointness side conditions
    below are its consequences rather than hypotheses of their own.  The one
    fact it cannot supply is where the closure is: that comes from whoever
    allocated it. *)
Lemma call_closure_spec :
  forall L f hs sr inst (z : Z) ret arg clos gmp tfi env a cl m rest,
  adapter_inv sr inst gmp ->
  smem sr inst = Some m ->
  lookup_N (inst_types inst) (ly_ty_closure L) = Some ty_closure ->
  scratch_ok 16 24 ret arg gmp ->
  (ret + 24 <= clos)%N ->
  (clos < i32mod)%N ->
  (tfi < i32mod)%N ->
  load_i32 m (clos + 4)%N = Some (VAL_int32 (N_to_i32 tfi)) ->
  load_i32 m (clos + 8)%N = Some (VAL_int32 (N_to_i32 env)) ->
  stab_elem sr inst tbl_funcs tfi = Some (VAL_ref_func a) ->
  lookup_N (s_funcs sr) a = Some cl ->
  cl_type cl = ty_closure ->
  call_target_spec inst f a ->
  (0 <= z < 2 ^ 63)%Z ->
  exists hs' sr' m',
    reduce_trans
      (hs, sr, exp_locs (Z_to_i64 z) ret arg clos inst,
       to_e_list (call_closure L) ++ rest)
      (hs', sr', exp_locs (Z_to_i64 z) ret arg clos inst, rest) /\
    smem sr' inst = Some m' /\
    (operations.mem_length m <= operations.mem_length m')%N /\
    mem_agree_below m m' (ret + 16)%N /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr /\
    ( (exists gmp' res,
         adapter_inv sr' inst gmp' /\
         sglob_val sr' inst glob_result = Some (N_to_value res) /\
         (res < i32mod)%N /\
         repr_prim m' gmp' res (f z))
      \/ sglob_val sr' inst glob_out_of_mem = Some (N_to_value 1) ).
Proof.
  intros L f hs sr inst z ret arg clos gmp tfi env a cl m rest
         Hinv Hmem Hty Hscr Hclosge Hclosmod Htfimod
         Hf4 Hf8 Htab Hcl Hclty Htgt Hz.
  (* the box sits above the return area and below the frontier, and the
     closure was allocated above it: that is the whole of what the stores
     below need, and all but the last of it comes out of [Hscr]. *)
  have Harg := sk_arg _ _ _ _ _ Hscr.
  have Hargmp := sk_box_below _ _ _ _ _ Hscr.
  have Hargmod := sk_arg_mod _ _ _ _ _ Hscr.
  have Heven := sk_even _ _ _ _ _ Hscr.
  destruct (ai_mem _ _ _ Hinv) as [m0 [Hmem0 Hgmplen]].
  assert (m0 = m) as -> by congruence.
  (* box the argument *)
  destruct (store_i64_exists m arg (VAL_int64 (Z_to_i64 z)) (ltac:(lia)))
    as [m1 Hst1].
  destruct (smem_store_i64_spec sr inst arg 0%N (VAL_int64 (Z_to_i64 z)) m m1 Hmem
              (ltac:(rewrite N.add_0_r; exact Hst1)))
    as [sr1 [Hsm1 [Hmem1 [Hg1 [Hf1 Ht1]]]]].
  assert (Hlen1 : operations.mem_length m1 = operations.mem_length m)
    by (eapply store_i64_length; exact Hst1).
  assert (Hinv1 : adapter_inv sr1 inst gmp)
    by (eapply adapter_inv_mem_swap; [exact Hinv | exact Hg1 | exact Hmem1 | lia]).
  (* the closure's fields are above the box, so they survive it *)
  assert (Hf4' : load_i32 m1 (clos + 4)%N = Some (VAL_int32 (N_to_i32 tfi))).
  { rewrite (load_i32_store_i64_disjoint m m1 arg (clos + 4)%N _ Hst1
               (ltac:(right; lia))). exact Hf4. }
  assert (Hf8' : load_i32 m1 (clos + 8)%N = Some (VAL_int32 (N_to_i32 env))).
  { rewrite (load_i32_store_i64_disjoint m m1 arg (clos + 8)%N _ Hst1
               (ltac:(right; lia))). exact Hf8. }
  assert (Htab1 : stab_elem sr1 inst tbl_funcs tfi = Some (VAL_ref_func a))
    by (rewrite (stab_elem_tables sr sr1 inst _ _ Ht1); exact Htab).
  assert (Hcl1 : lookup_N (s_funcs sr1) a = Some cl) by (rewrite Hf1; exact Hcl).
  (* the box, as the target's precondition wants it *)
  assert (Hbox : repr_prim m1 gmp arg z).
  { split; [lia|]. split; [exact Heven|].
    eapply load_i64_store_i64_same; exact Hst1. }
  (* the two fields, as [r_load_success] will read them *)
  unfold load_i32 in Hf4', Hf8'.
  destruct (load m1 (clos + 4)%N 0%N 4) as [bs4|] eqn:Hload4; [|discriminate].
  destruct (load m1 (clos + 8)%N 0%N 4) as [bs8|] eqn:Hload8; [|discriminate].
  assert (Hbs4 : wasm_deserialise bs4 T_i32 = VAL_int32 (N_to_i32 tfi))
    by congruence.
  assert (Hbs8 : wasm_deserialise bs8 T_i32 = VAL_int32 (N_to_i32 env))
    by congruence.
  destruct (Htgt hs sr1 (exp_locs (Z_to_i64 z) ret arg clos inst) m1 gmp env arg z
              (ltac:(reflexivity)) Hinv1 Hmem1 Hbox Hargmod Hz)
    as [hs' [sr' [m' [Hredt [Hmem' [Hlen' [Hagr' [Hfun' [Htabs' Hout]]]]]]]]].
  exists hs', sr', m'. split.
  { cbn. unfold store_i64_at, load_word.
    dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary_eliml 0 1. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary 2.
    { eapply r_store_success. cbn [f_inst memarg_offset].
      rewrite (N_of_uint_N_to_i32 _ Hargmod). exact Hsm1. }
    dostep_nary 0. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary 1.
    { eapply r_load_success; [exact Hmem1 |].
      rewrite (N_of_uint_N_to_i32 _ Hclosmod).
      change (tnum_length T_i32) with 4%N.
      rewrite load_word_offset. exact Hload8. }
    rewrite Hbs8.
    dostep_nary_eliml 0 1. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary_eliml 0 2. apply r_local_get. unfold exp_locs. reflexivity.
    dostep_nary_eliml 1 2.
    { eapply r_load_success; [exact Hmem1 |].
      rewrite (N_of_uint_N_to_i32 _ Hclosmod).
      change (tnum_length T_i32) with 4%N.
      rewrite load_word_offset. exact Hload4. }
    rewrite Hbs4.
    dostep_nary_eliml 1 2.
    { eapply r_call_indirect_success.
      - rewrite (N_of_uint_N_to_i32 _ Htfimod). exact Htab1.
      - exact Hcl1.
      - rewrite Hclty. exact Hty. }
    exact (reduce_trans_elimr rest _ _ _ _ _ _ _ _ Hredt). }
  split; [exact Hmem'|].
  split; [lia|].
  split.
  { eapply mem_agree_below_trans.
    - eapply store_i64_frame; [exact Hst1 | lia].
    - eapply mem_agree_below_weaken; [exact Hagr' | lia]. }
  split; [rewrite Hfun' Hf1; reflexivity|].
  split; [rewrite Htabs' Ht1; reflexivity|].
  destruct Hout as [[gmp' [res [Hinv' [Hgle [Hres [Hresmod Hrepr]]]]]] | Hoom].
  - left. exists gmp', res. split; [exact Hinv'|]. split; [exact Hres|].
    split; [exact Hresmod | exact Hrepr].
  - right. exact Hoom.
Qed.

(** ** The answer, as a function of the argument

    The export is deterministic except for the backend's right to run out of
    memory, so this is as tight as the statement can be. *)
Definition id63_answer (f : Z -> Z) (z : Z) (r : i64 + abi_error) : Prop :=
  if (z <? 2 ^ 63)%Z
  then r = inl (Z_to_i64 (f z)) \/ r = inr OutOfMemory
  else r = inr OutOfRange.

(** ** The theorem

    Calling the export on any [u64] terminates with a pointer to a return
    area holding the canonical encoding of the right answer.  The two [room]
    hypotheses rule out the [unreachable] trap — a memory too small for the
    24-byte scratch area has no return area to report into — and a 4GiB
    memory, whose size in bytes the 32-bit [room_check] cannot represent. *)
Theorem id63_export_correct :
  forall L fi f hs sr inst fr gmp m (z : Z),
  adapter_instance L fi sr inst ->
  main_certirocq_spec L f inst ->
  adapter_inv sr inst gmp ->
  smem sr inst = Some m ->
  f_inst fr = inst ->
  (alloc_ptr gmp 8 + 24 <= mem_size m * page_bytes)%N ->
  (mem_size m * page_bytes < i32mod)%N ->
  (0 <= z < 2 ^ 64)%Z ->
  exists hs' sr' m' ret r,
    reduce_trans
      (hs, sr, fr,
       [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z)));
           AI_basic (BI_call fi)])
      (hs', sr', fr, [:: AI_basic (i32c ret)]) /\
    smem sr' inst = Some m' /\
    canon_result_u64 m' ret r /\
    id63_answer f z r.
Proof.
  intros L fi f hs sr inst fr gmp m z [Hexp Hok Herr Hclosty] Hmain
         Hinv Hmem Hfinst Hroom Hszmod Hz.
  have [[Hplo Hphi] [k8 Hk8]] := alloc_ptr_bounds gmp 8 (ltac:(lia)).
  set (R := alloc_ptr gmp 8) in *.
  assert (Hcov : (mem_size m * page_bytes <= operations.mem_length m)%N).
  { unfold mem_size, page_bytes. change 65536%N with page_size.
    rewrite N.mul_comm. apply N.Div0.mul_div_le. }
  assert (Hslack : (R + 24 + 12 < i32mod)%N).
  { have Hi32 : i32mod = 4294967296%N by (unfold i32mod; vm_compute; reflexivity).
    unfold page_bytes in Hroom, Hszmod. rewrite Hi32 in Hszmod |- *. lia. }
  (* the prologue: the scratch area, and the three pointers into it *)
  destruct (prologue_room_spec hs sr inst (Z_to_i64 z) gmp m
              (to_e_list (range_check L)
               ++ [:: AI_basic (BI_call (ly_main L))]
               ++ to_e_list (oom_check L)
               ++ [:: AI_basic (BI_global_get glob_result);
                      AI_basic (BI_local_set 3%N)]
               ++ to_e_list (call_closure L)
               ++ to_e_list (oom_check L)
               ++ to_e_list (epilogue L))
              Hinv Hmem Hroom Hszmod)
    as [sr1 [Hred1 [Hinv1 [Hscr1 [Hmem1 [Hfun1 Htab1]]]]]].
  assert (Hpre1 : reduce_trans
    (hs, sr, fr,
     [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z))); AI_basic (BI_call fi)])
    (hs, sr1, fr,
     [:: AI_frame 1 (exp_locs (Z_to_i64 z) R (R + 16)%N 0 inst)
           [:: AI_label 1 [::]
                 (to_e_list (range_check L)
                  ++ [:: AI_basic (BI_call (ly_main L))]
                  ++ to_e_list (oom_check L)
                  ++ [:: AI_basic (BI_global_get glob_result);
                         AI_basic (BI_local_set 3%N)]
                  ++ to_e_list (call_closure L)
                  ++ to_e_list (oom_check L)
                  ++ to_e_list (epilogue L))]])).
  { eapply rt_trans; [exact (export_entry L fi hs sr inst fr _ Hexp Hfinst) |].
    rewrite to_e_list_export_body.
    apply (reduce_trans_frame 1 fr); apply reduce_trans_label1. exact Hred1. }
  (* the return area is inside the memory, and its address is representable *)
  assert (Hroom16 : (R + 16 <= operations.mem_length m)%N) by lia.
  assert (HRmod : (R < i32mod)%N) by lia.
  destruct (z <? 2 ^ 63)%Z eqn:Hcmp;
    [apply Z.ltb_lt in Hcmp | apply Z.ltb_ge in Hcmp].
  - (* in range: the check falls through, and [$main_function] runs *)
    assert (Hpre2 : reduce_trans
      (hs, sr, fr,
       [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z))); AI_basic (BI_call fi)])
      (hs, sr1, fr,
       [:: AI_frame 1 (exp_locs (Z_to_i64 z) R (R + 16)%N 0 inst)
             [:: AI_label 1 [::]
                   ([:: AI_basic (BI_call (ly_main L))]
                    ++ to_e_list (oom_check L)
                    ++ [:: AI_basic (BI_global_get glob_result);
                           AI_basic (BI_local_set 3%N)]
                    ++ to_e_list (call_closure L)
                    ++ to_e_list (oom_check L)
                    ++ to_e_list (epilogue L))]])).
    { eapply rt_trans; [exact Hpre1 |].
      apply (reduce_trans_frame 1 fr); apply reduce_trans_label1.
      apply range_check_pass. lia. }
    clear Hpre1 Hred1.
    destruct (Hmain hs sr1 (exp_locs (Z_to_i64 z) R (R + 16)%N 0 inst) (R + 24)%N m
                (ltac:(reflexivity)) Hinv1 Hmem1)
      as [hs2 [sr2 [m2 [Hredm [Hmem2 [Hlenm [Hagrm [Hfunm [Htabm Hout]]]]]]]]].
    assert (Hpre3 : reduce_trans
      (hs, sr, fr,
       [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z))); AI_basic (BI_call fi)])
      (hs2, sr2, fr,
       [:: AI_frame 1 (exp_locs (Z_to_i64 z) R (R + 16)%N 0 inst)
             [:: AI_label 1 [::]
                   (to_e_list (oom_check L)
                    ++ [:: AI_basic (BI_global_get glob_result);
                           AI_basic (BI_local_set 3%N)]
                    ++ to_e_list (call_closure L)
                    ++ to_e_list (oom_check L)
                    ++ to_e_list (epilogue L))]])).
    { eapply rt_trans; [exact Hpre2 |].
      apply (reduce_trans_frame 1 fr); apply reduce_trans_label1.
      exact (reduce_trans_elimr _ _ _ _ _ _ _ _ _ Hredm). }
    clear Hpre2.
    destruct Hout as [[gmp2 [clos [tfi [env [a [cl [Hinv2 [Hgle [Hres2
       [Hclosge [Hclos12 [Hclosmod [Hf4 [Hf8 [Htfimod [Htabe [Hcla
       [Hclty Htgt]]]]]]]]]]]]]]]]]] | Hoom1].
    + (* the program ran: box the argument and call the closure *)
      assert (Hpre4 : reduce_trans
        (hs, sr, fr,
         [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z)));
             AI_basic (BI_call fi)])
        (hs2, sr2, fr,
         [:: AI_frame 1 (exp_locs (Z_to_i64 z) R (R + 16)%N clos inst)
               [:: AI_label 1 [::]
                     (to_e_list (call_closure L)
                      ++ to_e_list (oom_check L)
                      ++ to_e_list (epilogue L))]])).
      { eapply rt_trans; [exact Hpre3 |].
        apply (reduce_trans_frame 1 fr); apply reduce_trans_label1.
        eapply rt_trans;
          [apply oom_check_pass; exact (ai_oom _ _ _ Hinv2) |].
        dostep_nary 0. apply r_global_get. exact Hres2.
        dostep_nary 1.
        { eapply r_local_set with
            (v := N_to_value clos)
            (f' := exp_locs (Z_to_i64 z) R (R + 16)%N clos inst)
            (vd := N_to_value 0);
            [reflexivity | unfold exp_locs; cbn; reflexivity
             | unfold exp_locs; cbn; reflexivity]. }
        apply rt_refl. }
      clear Hpre3.
      (* the backend only ever moved the frontier up, so the scratch area
         the prologue carved out is still described by [scratch_ok] *)
      assert (Hscr2 : scratch_ok 16 24 R (R + 16)%N gmp2).
      { eapply scratch_ok_gmp_mono;
          [exact Hscr1 | exact Hgle
           | have := ai_no_wrap _ _ _ Hinv2; lia]. }
      (* [$main_function] allocated the closure above the scratch area *)
      assert (Hclosabove : (R + 24 <= clos)%N) by exact Hclosge.
      destruct (call_closure_spec L f hs2 sr2 inst z R (R + 16)%N clos gmp2
                  tfi env a cl m2
                  (to_e_list (oom_check L) ++ to_e_list (epilogue L))
                  Hinv2 Hmem2 Hclosty Hscr2 Hclosabove
                  Hclosmod Htfimod Hf4 Hf8 Htabe Hcla Hclty Htgt
                  (ltac:(lia)))
        as [hs3 [sr3 [m3 [Hredc [Hmem3 [Hlenc [Hagrc [Hfunc3 [Htabc Houtc]]]]]]]]].
      assert (Hpre5 : reduce_trans
        (hs, sr, fr,
         [:: AI_basic (BI_const_num (VAL_int64 (Z_to_i64 z)));
             AI_basic (BI_call fi)])
        (hs3, sr3, fr,
         [:: AI_frame 1 (exp_locs (Z_to_i64 z) R (R + 16)%N clos inst)
               [:: AI_label 1 [::]
                     (to_e_list (oom_check L) ++ to_e_list (epilogue L))]])).
      { eapply rt_trans; [exact Hpre4 |].
        apply (reduce_trans_frame 1 fr); apply reduce_trans_label1.
        exact Hredc. }
      clear Hpre4.
      destruct Houtc as [[gmp3 [res [Hinv3 [Hres3 [Hresmod Hrepr]]]]] | Hoom2].
      * (* the answer is in the heap: write it into the return area *)
        destruct Hrepr as [_ [_ Hpayload]].
        destruct (epilogue_spec L hs3 sr3 inst (Z_to_i64 z) R (R + 16)%N clos
                    res (Z_to_i64 (f z)) m3
                    (native_at_funcs sr sr3 inst _ _ _ (ltac:(congruence)) Hok)
                    Hmem3 Hres3 Hresmod Hpayload (ltac:(lia)) HRmod)
          as [sr4 [m4 [Hrede [Hmem4 [_ [_ [_ [_ [_ Hcanon]]]]]]]]].
        exists hs3, sr4, m4, R, (inl (Z_to_i64 (f z))).
        split.
        { eapply rt_trans; [exact Hpre5 |].
          eapply rt_trans;
            [apply (reduce_trans_frame 1 fr); apply reduce_trans_label1;
             eapply rt_trans;
               [apply oom_check_pass; exact (ai_oom _ _ _ Hinv3)
                | exact Hrede] |].
          apply export_exit. }
        split; [exact Hmem4 |].
        split; [exact Hcanon |].
        unfold id63_answer. rewrite (ltac:(apply Z.ltb_lt; exact Hcmp)
                                      : (z <? 2 ^ 63)%Z = true).
        left. reflexivity.
      * (* the closure's body ran out of memory *)
        destruct (oom_check_fail L hs3 sr3 inst fr (Z_to_i64 z) R (R + 16)%N clos
                    (to_e_list (epilogue L)) m3
                    (native_at_funcs sr sr3 inst _ _ _ (ltac:(congruence)) Herr)
                    Hoom2 Hmem3 (ltac:(lia)) HRmod)
          as [sr4 [m4 [Hredf [Hmem4 [_ [_ [_ [_ [_ Hcanon]]]]]]]]].
        exists hs3, sr4, m4, R, (inr OutOfMemory).
        split; [eapply rt_trans; [exact Hpre5 | exact Hredf] |].
        split; [exact Hmem4 |].
        split; [exact Hcanon |].
        unfold id63_answer. rewrite (ltac:(apply Z.ltb_lt; exact Hcmp)
                                      : (z <? 2 ^ 63)%Z = true).
        right. reflexivity.
    + (* [$main_function] ran out of memory *)
      destruct (oom_check_fail L hs2 sr2 inst fr (Z_to_i64 z) R (R + 16)%N 0
                  ([:: AI_basic (BI_global_get glob_result);
                       AI_basic (BI_local_set 3%N)]
                   ++ to_e_list (call_closure L)
                   ++ to_e_list (oom_check L)
                   ++ to_e_list (epilogue L)) m2
                  (native_at_funcs sr sr2 inst _ _ _ (ltac:(congruence)) Herr)
                  Hoom1 Hmem2 (ltac:(lia)) HRmod)
        as [sr3 [m3 [Hredf [Hmem3 [_ [_ [_ [_ [_ Hcanon]]]]]]]]].
      exists hs2, sr3, m3, R, (inr OutOfMemory).
      split; [eapply rt_trans; [exact Hpre3 | exact Hredf] |].
      split; [exact Hmem3 |].
      split; [exact Hcanon |].
      unfold id63_answer. rewrite (ltac:(apply Z.ltb_lt; exact Hcmp)
                                    : (z <? 2 ^ 63)%Z = true).
      right. reflexivity.
  - (* out of range: the check answers [err] and returns *)
    destruct (range_check_fail L hs sr1 inst fr z R (R + 16)%N 0
                ([:: AI_basic (BI_call (ly_main L))]
                 ++ to_e_list (oom_check L)
                 ++ [:: AI_basic (BI_global_get glob_result);
                        AI_basic (BI_local_set 3%N)]
                 ++ to_e_list (call_closure L)
                 ++ to_e_list (oom_check L)
                 ++ to_e_list (epilogue L)) m
                (native_at_funcs _ _ _ _ _ _ Hfun1 Herr)
                Hmem1 Hroom16 HRmod (ltac:(lia)))
      as [sr2 [m2 [Hredf [Hmem2 [_ [_ [_ [_ [_ Hcanon]]]]]]]]].
    exists hs, sr2, m2, R, (inr OutOfRange).
    split; [eapply rt_trans; [exact Hpre1 | exact Hredf] |].
    split; [exact Hmem2 |].
    split; [exact Hcanon |].
    unfold id63_answer. rewrite (ltac:(apply Z.ltb_ge; exact Hcmp)
                                  : (z <? 2 ^ 63)%Z = false).
    reflexivity.
Qed.

End Correctness.
