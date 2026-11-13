(** * The id63 adapter implements its WIT contract

    The theorem this slice exists to make: the adapter module that
    [componentize-formal generate] emitted for

      export id63: func(val: u64) -> result<u64, error>

    ([id63_adapter.v]) writes the canonical-ABI return area of [id63_abi.v]
    correctly, relative to two hypotheses about the CertiRocq-compiled
    backend it is instantiated against ([main_spec] and [call_target_spec]
    below) — the id63 instance of the *primitive export contract*.

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

From wasmcomponents Require Import wasm_steps wasm_mem certirocq_heap certirocq_inv
  id63_abi id63_adapter.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.

(** ** i32/i64 arithmetic the export needs

    Stated at the level of [app_binop]/[app_relop], like the lemmas in
    [certirocq_inv.v] they extend. *)

Lemma app_binop_mul : forall a b,
  app_binop (Binop_i BOI_mul) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b))
  = Some (VAL_int32 (N_to_i32 (a * b)%N)).
Proof.
  intros a b.
  change (app_binop (Binop_i BOI_mul) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b)))
    with (Some (VAL_int32 (Wasm_int.Int32.mul (N_to_i32 a) (N_to_i32 b)))).
  do 2! f_equal. unfold Wasm_int.Int32.mul, N_to_i32. rewrite N2Z.inj_mul.
  apply Wasm_int.Int32.eqm_samerepr, Wasm_int.Int32.eqm_mult;
    apply Wasm_int.Int32.eqm_unsigned_repr_l, Wasm_int.Int32.eqm_refl.
Qed.

(** [i32.const -8] is the all-ones mask with the low three bits cleared,
    which is the shape [app_binop_align_up] wants it in. *)
Lemma i32_neg8_mask :
  Wasm_int.Int32.repr (-8)%Z
  = Wasm_int.Int32.xor (N_to_i32 (8 - 1)%N) Wasm_int.Int32.mone.
Proof. apply Wasm_int.Int32.same_if_eq. vm_compute. reflexivity. Qed.

(** The prologue's rounding of the allocation pointer, as one binop. *)
Lemma app_binop_align8 : forall gmp,
  (gmp + 8 < i32mod)%N ->
  app_binop (Binop_i BOI_and)
    (VAL_int32 (N_to_i32 (gmp + 7)%N))
    (VAL_int32 (Wasm_int.Int32.repr (-8)%Z))
  = Some (VAL_int32 (N_to_i32 (alloc_ptr gmp 8))).
Proof.
  intros gmp H. rewrite i32_neg8_mask.
  replace (gmp + 7)%N with (gmp + (8 - 1))%N by lia.
  apply app_binop_align_up; [right; right; reflexivity | exact H].
Qed.

(** The range check reads the [u64] argument as a signed [i64]: negative
    means the top bit is set, i.e. the value has no [Uint63.int]. *)

Lemma i64_max_signed_val : Wasm_int.Int64.max_signed = (2 ^ 63 - 1)%Z.
Proof. vm_compute. reflexivity. Qed.

Lemma i64_min_signed_val : Wasm_int.Int64.min_signed = (- 2 ^ 63)%Z.
Proof. vm_compute. reflexivity. Qed.

Lemma i64_modulus_val : Wasm_int.Int64.modulus = (2 ^ 64)%Z.
Proof. vm_compute. reflexivity. Qed.

Lemma i64_half_modulus_val : Wasm_int.Int64.half_modulus = (2 ^ 63)%Z.
Proof. vm_compute. reflexivity. Qed.

Lemma app_relop_lt0_s64_false : forall z,
  (0 <= z < 2 ^ 63)%Z ->
  app_relop (Relop_i (ROI_lt SX_S))
    (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0))
  = false.
Proof.
  intros z Hz.
  change (app_relop (Relop_i (ROI_lt SX_S))
            (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0)))
    with (Wasm_int.Int64.lt (Z_to_i64 z) (Z_to_i64 0)).
  unfold Wasm_int.Int64.lt, Z_to_i64.
  rewrite !Wasm_int.Int64.signed_repr;
    try (rewrite i64_min_signed_val i64_max_signed_val; lia).
  destruct (Coqlib.zlt z 0); [lia | reflexivity].
Qed.

Lemma app_relop_lt0_s64_true : forall z,
  (2 ^ 63 <= z < 2 ^ 64)%Z ->
  app_relop (Relop_i (ROI_lt SX_S))
    (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0))
  = true.
Proof.
  intros z Hz.
  change (app_relop (Relop_i (ROI_lt SX_S))
            (VAL_int64 (Z_to_i64 z)) (VAL_int64 (Z_to_i64 0)))
    with (Wasm_int.Int64.lt (Z_to_i64 z) (Z_to_i64 0)).
  unfold Wasm_int.Int64.lt, Z_to_i64.
  rewrite (Wasm_int.Int64.signed_repr 0);
    [| rewrite i64_min_signed_val i64_max_signed_val; lia].
  rewrite Wasm_int.Int64.signed_repr_eq.
  rewrite Z.mod_small; [| rewrite i64_modulus_val; lia].
  rewrite i64_half_modulus_val i64_modulus_val.
  destruct (Coqlib.zlt z (2 ^ 63)); [lia |].
  destruct (Coqlib.zlt (z - 2 ^ 64) 0); [reflexivity | lia].
Qed.

(** ** Storing and reading single bytes

    The return area's discriminants are bytes, written with [i32.store8] and
    read back with [load_u8].  [store_packed] is [store] at length 1, which
    keeps only the low byte of the operand — little-endian, by the vendored
    compcert's [Archi]. *)

Lemma bytes_takefill_takefill : forall b l (bs : bytes),
  bytes_takefill b l (bytes_takefill b l bs) = bytes_takefill b l bs.
Proof.
  intros b l bs. apply bytes_takefill_id. apply bytes_takefill_length.
Qed.

Section WithMemory.

Context `{Memory}.

Lemma store_takefill : forall (m : meminst) (a off : N) (bs : bytes) (l : nat),
  store m a off (bytes_takefill #00 l bs) l = store m a off bs l.
Proof.
  intros m a off bs l. unfold store.
  destruct (N.leb (a + off + N.of_nat l) (mem_length m)); [| reflexivity].
  rewrite bytes_takefill_takefill. reflexivity.
Qed.

(** The two discriminant values the adapter ever stores. *)
Lemma takefill_serialise_0 :
  bytes_takefill #00 1 (serialise_i32 (N_to_i32 0)) = [:: Integers.Byte.zero].
Proof. vm_compute. reflexivity. Qed.

Lemma takefill_serialise_1 :
  bytes_takefill #00 1 (serialise_i32 (N_to_i32 1)) = [:: Integers.Byte.one].
Proof. vm_compute. reflexivity. Qed.

(** Writing the low byte of [N_to_i32 k] and reading it back, for the [k]
    the adapter uses.  Stated for 0 and 1 only because the general statement
    needs [k mod 256] and nothing here does. *)
Lemma store_u8_load_01 : forall (m m' : meminst) (a k : N),
  (k = 0 \/ k = 1)%N ->
  store m a 0%N (serialise_i32 (N_to_i32 k)) 1 = Some m' ->
  load_u8 m' a = Some k.
Proof.
  intros m m' a k Hk Hst.
  rewrite <- store_takefill in Hst.
  assert (Hld : load m' a 0%N (N.of_nat 1)
                = Some (bytes_takefill #00 1 (serialise_i32 (N_to_i32 k)))).
  { eapply load_store_same; [exact Hst | apply bytes_takefill_length]. }
  change (N.of_nat 1) with 1%N in Hld.
  unfold load_u8.
  destruct Hk as [-> | ->].
  - rewrite takefill_serialise_0 in Hld. rewrite Hld. vm_compute. reflexivity.
  - rewrite takefill_serialise_1 in Hld. rewrite Hld. vm_compute. reflexivity.
Qed.

(** A one-byte store that fits succeeds. *)
Lemma store_u8_exists : forall (m : meminst) (a : N) (c : i32),
  (a + 1 <= operations.mem_length m)%N ->
  exists m', store m a 0%N (serialise_i32 c) 1 = Some m'.
Proof. intros m a c Ha. apply store_exists. lia. Qed.

Lemma store_u8_length : forall (m m' : meminst) (a : N) (bs : bytes),
  store m a 0%N bs 1 = Some m' ->
  operations.mem_length m' = operations.mem_length m.
Proof. intros m m' a bs Hs. eapply store_length; exact Hs. Qed.

(** Byte reads are oblivious to disjoint stores of any width. *)
Lemma load_u8_store_disjoint : forall (m m' : meminst) (a b : N) (bs : bytes) (l : nat),
  store m a 0%N bs l = Some m' ->
  ((b + 1 <= a)%N \/ (a + N.of_nat l <= b)%N) ->
  load_u8 m' b = load_u8 m b.
Proof.
  intros m m' a b bs l Hs Hdis. unfold load_u8.
  assert (Hld : load m' b 0%N (N.of_nat 1) = load m b 0%N (N.of_nat 1)).
  { eapply load_store_disjoint; [exact Hs | cbn; lia]. }
  change (N.of_nat 1) with 1%N in Hld. rewrite Hld. reflexivity.
Qed.

(** And conversely for the loads the epilogue does below a byte store. *)
Lemma load_i32_store_disjoint_gen : forall (m m' : meminst) (a b : N) (bs : bytes) (l : nat),
  store m a 0%N bs l = Some m' ->
  ((b + 4 <= a)%N \/ (a + N.of_nat l <= b)%N) ->
  load_i32 m' b = load_i32 m b.
Proof.
  intros m m' a b bs l Hs Hdis. unfold load_i32.
  assert (Hld : load m' b 0%N (N.of_nat 4) = load m b 0%N (N.of_nat 4)).
  { eapply load_store_disjoint; [exact Hs | cbn; lia]. }
  change (N.of_nat 4) with 4%N in Hld. rewrite Hld. reflexivity.
Qed.

Lemma load_u8_store_i64_disjoint : forall (m m' : meminst) (a b : N) (v : value_num),
  store_i64 m a v = Some m' ->
  ((b + 1 <= a)%N \/ (a + 8 <= b)%N) ->
  load_u8 m' b = load_u8 m b.
Proof.
  intros m m' a b v Hs Hdis. unfold store_i64 in Hs.
  eapply load_u8_store_disjoint; [exact Hs | cbn; lia].
Qed.

Lemma load_i32_store_i64_disjoint : forall (m m' : meminst) (a b : N) (v : value_num),
  store_i64 m a v = Some m' ->
  ((b + 4 <= a)%N \/ (a + 8 <= b)%N) ->
  load_i32 m' b = load_i32 m b.
Proof.
  intros m m' a b v Hs Hdis. unfold store_i64 in Hs.
  eapply load_i32_store_disjoint_gen; [exact Hs | cbn; lia].
Qed.

(** The static offset of a [memarg], moved into the address, for the 8-byte
    width ([certirocq_inv.v] has the 4-byte one). *)
Lemma load_i64_offset : forall (m : meminst) (addr off : N),
  load m addr off 8 = load m (addr + off)%N 0%N 8.
Proof.
  intros m addr off. unfold load.
  replace (addr + (off + 8))%N with (addr + off + (0 + 8))%N by lia.
  rewrite N.add_0_r. reflexivity.
Qed.

End WithMemory.

Section Correctness.

Context `{ho : host}.

(** ** The store-record level of the two stores

    Like [smem_store_spec] in [certirocq_inv.v], with two additions: the
    packed (byte) width, and the [s_tables] clause — the argument store in
    [call_closure] happens after the table entry for the closure is known,
    and the entry has to survive it. *)

Lemma smem_store_packed_u8_spec : forall sr inst a off (c : i32) m m',
  smem sr inst = Some m ->
  store m (a + off)%N 0%N (serialise_i32 c) 1 = Some m' ->
  exists sr',
    smem_store_packed sr inst a off (VAL_int32 c) Tp_i8 = Some sr' /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr.
Proof.
  intros sr inst a off c m m' Hmem Hst.
  rewrite <- store_offset in Hst.
  unfold smem in Hmem. unfold smem_store_packed, store_packed.
  destruct (lookup_N (inst_mems inst) 0%N) as [addr|] eqn:Haddr; [|discriminate].
  rewrite Hmem.
  assert (Hst' : store m a off (serialise_num (VAL_int32 c))
                   (ssrnat.nat_of_bin (tp_length Tp_i8)) = Some m')
    by exact Hst.
  rewrite Hst'.
  eexists. split; [reflexivity|]. split; [| repeat split; reflexivity].
  unfold smem. rewrite Haddr. cbn. unfold lookup_N.
  rewrite nth_error_set_eq. reflexivity.
Qed.

Lemma smem_store_i64_spec : forall sr inst a off v m m',
  smem sr inst = Some m ->
  store_i64 m (a + off)%N v = Some m' ->
  exists sr',
    smem_store sr inst a off v T_i64 = Some sr' /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr /\
    s_tables sr' = s_tables sr.
Proof.
  intros sr inst a off v m m' Hmem Hst.
  unfold store_i64 in Hst. rewrite <- store_offset in Hst.
  unfold smem in Hmem. unfold smem_store.
  destruct (lookup_N (inst_mems inst) 0%N) as [addr|] eqn:Haddr; [|discriminate].
  rewrite Hmem.
  assert (Hst' : store m a off (serialise_num v)
                   (ssrnat.nat_of_bin (tnum_length T_i64)) = Some m')
    by exact Hst.
  rewrite Hst'.
  eexists. split; [reflexivity|]. split; [| repeat split; reflexivity].
  unfold smem. rewrite Haddr. cbn. unfold lookup_N.
  rewrite nth_error_set_eq. reflexivity.
Qed.

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

(** ** Store plumbing

    Two small gaps in [certirocq_inv.v]'s toolkit: a [global.set] leaves the
    tables alone, and [stab_elem] only looks at them. *)

Lemma supdate_glob_tables : forall sr inst j v sr',
  supdate_glob sr inst j v = Some sr' -> s_tables sr' = s_tables sr.
Proof.
  intros sr inst j v sr' Hupd.
  unfold supdate_glob, sglob_ind in Hupd.
  destruct (lookup_N (inst_globals inst) j) as [a|]; [|discriminate].
  cbn in Hupd. unfold supdate_glob_s in Hupd.
  destruct (lookup_N (s_globals sr) a) as [g|]; [|discriminate].
  cbn in Hupd. injection Hupd as <-. reflexivity.
Qed.

Lemma stab_elem_tables : forall sr sr' inst x i,
  s_tables sr' = s_tables sr ->
  stab_elem sr' inst x i = stab_elem sr inst x i.
Proof.
  intros sr sr' inst x i Ht. unfold stab_elem, stab. rewrite Ht. reflexivity.
Qed.

(** [adapter_inv] does not look at the memory's contents, so it survives any
    store that keeps the globals and the memory's length. *)
Lemma adapter_inv_mem_swap : forall sr sr' inst gmp (m' : meminst),
  adapter_inv sr inst gmp ->
  s_globals sr' = s_globals sr ->
  smem sr' inst = Some m' ->
  (gmp <= operations.mem_length m')%N ->
  adapter_inv sr' inst gmp.
Proof.
  intros sr sr' inst gmp m' Hinv Hg Hm Hlen. constructor.
  - rewrite (sglob_val_globals sr sr' inst glob_mem_ptr Hg).
    apply (ai_gmp _ _ _ Hinv).
  - apply (ai_gmp_even _ _ _ Hinv).
  - rewrite (sglob_val_globals sr sr' inst glob_out_of_mem Hg).
    apply (ai_oom _ _ _ Hinv).
  - exists m'. split; [exact Hm | exact Hlen].
  - apply (ai_no_wrap _ _ _ Hinv).
  - apply (ai_glob_distinct _ _ _ Hinv).
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
    [glob_out_of_mem].  [main_spec]: [$main_function] leaves the export's
    closure in [glob_result] — or signals; the closure's two fields and its
    table entry come with it, as does the behaviour of the entry. *)
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

Definition main_spec (L : layout) (f : Z -> Z) (inst : moduleinst) : Prop :=
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
                          (to_e_list (fail_with L (error_code e)))]
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
  split; [exact Hmemb|].
  split.
  { rewrite (supdate_glob_funcs _ _ _ _ _ Hupd2)
            (supdate_glob_funcs _ _ _ _ _ Hupd1). reflexivity. }
  rewrite (supdate_glob_tables _ _ _ _ _ Hupd2)
          (supdate_glob_tables _ _ _ _ _ Hupd1). reflexivity.
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
  main_spec L f inst ->
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
Admitted.

End Correctness.
