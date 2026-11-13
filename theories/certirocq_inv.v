(** * The backend's invariant, and the toolkit for stepping an adapter

    Everything an adapter proof needs that is not about a particular adapter:
    how i32 arithmetic in [N] relates to WasmCert's [Wasm_int.Int32], which
    fragment of CertiRocq's [INV] an adapter reads and writes, and the handful
    of store lemmas that turn a [global.set] or an [i32.store] into a fact
    about [sglob_val] and [load_i32].

    It was factored out of [examples/echo/echo_correct.v] when the second
    adapter ([examples/id63/]) needed the same material; the two slices share
    it unchanged.

    The real [INV] (CertiRocq [LambdaANF_to_Wasm_correct.v]) is a far larger
    conjunction than [adapter_inv] below; the bridge lemma owed when CertiRocq
    becomes a dependency is that it implies this one. *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics memory opsem host properties.

From Stdlib Require Import ZArith BinNat NArith.Nnat List Lia
  Relations.Relations Relations.Relation_Operators.

From wasmcomponents Require Import wasm_steps wasm_mem certirocq_heap.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.

(** ** i32 arithmetic

    The two loops in the adapter do exactly two numeric things: test the low
    bit of a value to tell an immediate from a pointer, and increment a
    counter.  So this is the whole arithmetic content of their proofs, stated
    at the level of [app_binop] so that the stepping proofs can discharge
    [rs_binop_success] by [apply] without having to normalise anything.

    [app_binop_add_one] needs no range hypothesis: [Wasm_int.Int32.repr]
    depends on its argument only modulo [modulus], so incrementing in [N] and
    incrementing in i32 agree even when the counter wraps.
    [app_binop_and_one] does need one, because it reads its argument back out
    with [unsigned]; every caller gets it from [ai_no_wrap]. *)

(** [cbn] must not look inside [N_to_i32]: the stepping proofs rely on [cbn] to
    expand instruction lists, and an i32 constant unfolded all the way to
    [Wasm_int.Int32.mkint] stops every lemma below from applying. *)
Arguments N_to_i32 : simpl never.

Lemma one_lt_i32mod : (1 < i32mod)%N.
Proof. unfold i32mod. vm_compute. reflexivity. Qed.

Lemma N_to_i32_one : N_to_i32 1 = Wasm_int.Int32.one.
Proof. reflexivity. Qed.

Lemma N_to_i32_zero : N_to_i32 0 = Wasm_int.int_zero i32m.
Proof. reflexivity. Qed.

Lemma N_to_i32_one_neq_zero : N_to_i32 1 <> Wasm_int.int_zero i32m.
Proof. rewrite N_to_i32_one. exact Wasm_int.Int32.one_not_zero. Qed.

Lemma unsigned_N_to_i32 : forall a, (a < i32mod)%N ->
  Wasm_int.Int32.unsigned (N_to_i32 a) = Z.of_N a.
Proof.
  intros a Ha. unfold N_to_i32.
  rewrite Wasm_int.Int32.unsigned_repr; [reflexivity|].
  unfold i32mod in Ha. unfold Wasm_int.Int32.max_unsigned.
  have Hp := Wasm_int.Int32.modulus_pos.
  apply N2Z.inj_lt in Ha. rewrite Z2N.id in Ha; [|lia]. lia.
Qed.

(** The address an [i32] denotes, as [r_load_success] computes it. *)
Lemma N_of_uint_N_to_i32 : forall a, (a < i32mod)%N ->
  Wasm_int.N_of_uint i32m (N_to_i32 a) = a.
Proof.
  intros a Ha.
  change (Wasm_int.N_of_uint i32m (N_to_i32 a))
    with (Z.to_N (Wasm_int.Int32.unsigned (N_to_i32 a))).
  rewrite unsigned_N_to_i32; [|assumption]. apply N2Z.id.
Qed.

Lemma Zland_1 : forall z, Z.land z 1 = (z mod 2)%Z.
Proof.
  intros z. replace 1%Z with (Z.ones 1) by reflexivity.
  rewrite Z.land_ones; [|lia]. now rewrite Z.pow_1_r.
Qed.

(** Masking off the tag bit: [and_ 1] is the test that tells an immediate from
    a pointer, and this is the only reason the proofs need a range bound. *)
Lemma app_binop_and_one : forall a, (a < i32mod)%N ->
  app_binop (Binop_i BOI_and) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 1))
  = Some (VAL_int32 (N_to_i32 (a mod 2)%N)).
Proof.
  intros a Ha.
  change (app_binop (Binop_i BOI_and) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 1)))
    with (Some (VAL_int32 (Wasm_int.Int32.and (N_to_i32 a) (N_to_i32 1)))).
  f_equal. f_equal.
  unfold Wasm_int.Int32.and.
  rewrite N_to_i32_one Wasm_int.Int32.unsigned_one.
  rewrite unsigned_N_to_i32; [|assumption].
  rewrite Zland_1. unfold N_to_i32. rewrite N2Z.inj_mod. reflexivity.
Qed.

(** Incrementing the counter.  No range hypothesis: [Wasm_int.Int32.repr]
    depends on its argument only modulo [modulus], so incrementing in [N] and
    incrementing in i32 agree even where the counter would wrap. *)
Lemma app_binop_add_one : forall a,
  app_binop (Binop_i BOI_add) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 1))
  = Some (VAL_int32 (N_to_i32 (a + 1)%N)).
Proof.
  intros a.
  change (app_binop (Binop_i BOI_add) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 1)))
    with (Some (VAL_int32 (Wasm_int.Int32.add (N_to_i32 a) (N_to_i32 1)))).
  f_equal. f_equal.
  unfold Wasm_int.Int32.add.
  rewrite N_to_i32_one Wasm_int.Int32.unsigned_one.
  unfold N_to_i32. rewrite N2Z.inj_add.
  apply Wasm_int.Int32.eqm_samerepr, Wasm_int.Int32.eqm_add.
  - apply Wasm_int.Int32.eqm_unsigned_repr_l, Wasm_int.Int32.eqm_refl.
  - apply Wasm_int.Int32.eqm_refl.
Qed.

(** ** More i32 arithmetic, for [$alloc]

    [$alloc] does the only genuinely bit-level computation in the adapter:
    it rounds the allocation pointer up to [align] with

      p = (gmp + (align-1)) & ~(align-1)

    and it converts between pages and bytes with [shl]/[shr_u] by 16.  As
    above, everything is stated at the level of [app_binop] / [app_relop] so
    that [rs_binop_success] and [rs_relop] close by [apply].

    The align-up identity is routed through compcert: [and x (not mask)] is
    [sub x (and x mask)] by [add_and], and [and x mask] is [modu x align] by
    [modu_and] once [align] is seen to be a power of two.  The upshot,
    [app_binop_align_up], gives the result as [align * ((gmp + align - 1) /
    align)] — the form from which both the alignment and the bounds
    [gmp <= p <= gmp + align - 1] follow by [N.Div0.div_mod] and [lia]. *)
Lemma i32_add_cancel_l : forall a b,
  Wasm_int.Int32.sub (Wasm_int.Int32.add a b) a = b.
Proof.
  intros a b. rewrite Wasm_int.Int32.sub_add_l Wasm_int.Int32.sub_idem.
  apply Wasm_int.Int32.add_zero_l.
Qed.

Lemma and_xor_mone : forall x mask,
  Wasm_int.Int32.and x (Wasm_int.Int32.xor mask Wasm_int.Int32.mone)
  = Wasm_int.Int32.sub x (Wasm_int.Int32.and x mask).
Proof.
  intros x mask.
  change (Wasm_int.Int32.xor mask Wasm_int.Int32.mone)
    with (Wasm_int.Int32.not mask).
  rewrite <- (i32_add_cancel_l (Wasm_int.Int32.and x mask)
                               (Wasm_int.Int32.and x (Wasm_int.Int32.not mask))) at 1.
  f_equal.
  rewrite Wasm_int.Int32.add_and; [| apply Wasm_int.Int32.and_not_self ].
  rewrite Wasm_int.Int32.or_not_self. apply Wasm_int.Int32.and_mone.
Qed.

Lemma N_to_i32_sub_one : forall a, (1 <= a)%N -> (a < i32mod)%N ->
  Wasm_int.Int32.sub (N_to_i32 a) Wasm_int.Int32.one = N_to_i32 (a - 1).
Proof.
  intros a H1 H2. unfold Wasm_int.Int32.sub.
  rewrite unsigned_N_to_i32; [|assumption].
  rewrite Wasm_int.Int32.unsigned_one.
  unfold N_to_i32. f_equal. lia.
Qed.

Lemma N_to_i32_sub : forall a b, (b <= a)%N -> (a < i32mod)%N ->
  Wasm_int.Int32.sub (N_to_i32 a) (N_to_i32 b) = N_to_i32 (a - b).
Proof.
  intros a b H1 H2. unfold Wasm_int.Int32.sub.
  assert (Hua : Wasm_int.Int32.unsigned (N_to_i32 a) = Z.of_N a)
    by (apply unsigned_N_to_i32; lia).
  assert (Hub : Wasm_int.Int32.unsigned (N_to_i32 b) = Z.of_N b)
    by (apply unsigned_N_to_i32; lia).
  rewrite Hua Hub. unfold N_to_i32. f_equal. lia.
Qed.

Lemma is_power2_align : forall align, (align = 1 \/ align = 4 \/ align = 8)%N ->
  exists logn, Wasm_int.Int32.is_power2 (N_to_i32 align) = Some logn.
Proof.
  intros align Hal.
  destruct Hal as [E|[E|E]]; subst align; eexists; reflexivity.
Qed.

Lemma and_mask_mod : forall x align,
  (align = 1 \/ align = 4 \/ align = 8)%N ->
  (x < i32mod)%N ->
  Wasm_int.Int32.and (N_to_i32 x) (N_to_i32 (align - 1))
  = N_to_i32 (x mod align)%N.
Proof.
  intros x align Hal Hx.
  have [logn Hpow] := is_power2_align _ Hal.
  assert (Hal1 : (1 <= align)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  assert (Hal2 : (align < i32mod)%N) by (destruct Hal as [E|[E|E]]; subst align; unfold i32mod; vm_compute; reflexivity).
  rewrite <- (N_to_i32_sub_one align) by assumption.
  rewrite <- (Wasm_int.Int32.modu_and _ _ _ Hpow).
  unfold Wasm_int.Int32.modu.
  assert (Hux : Wasm_int.Int32.unsigned (N_to_i32 x) = Z.of_N x)
    by (apply unsigned_N_to_i32; lia).
  assert (Hua : Wasm_int.Int32.unsigned (N_to_i32 align) = Z.of_N align)
    by (apply unsigned_N_to_i32; lia).
  rewrite Hux Hua. unfold N_to_i32. f_equal. rewrite N2Z.inj_mod. reflexivity.
Qed.

Lemma i32mod_val : i32mod = 4294967296%N.
Proof. unfold i32mod. vm_compute. reflexivity. Qed.

Lemma Zof_i32mod : Z.of_N i32mod = Wasm_int.Int32.modulus.
Proof. unfold i32mod. rewrite Z2N.id; [reflexivity|]. vm_compute. discriminate. Qed.

Lemma i32_as_N : forall c, c = N_to_i32 (Z.to_N (Wasm_int.Int32.unsigned c)).
Proof.
  intros c. unfold N_to_i32.
  rewrite Z2N.id; [| have := Wasm_int.Int32.unsigned_range c; lia ].
  symmetry. apply Wasm_int.Int32.repr_unsigned.
Qed.

Lemma i32mod_Z : Wasm_int.Int32.modulus = 4294967296%Z.
Proof. vm_compute. reflexivity. Qed.

Lemma i32_as_N_lt : forall c, (Z.to_N (Wasm_int.Int32.unsigned c) < i32mod)%N.
Proof.
  intros c. have H := Wasm_int.Int32.unsigned_range c.
  rewrite i32mod_Z in H. rewrite i32mod_val. lia.
Qed.

Lemma to_N_unsigned_repr : forall z, (0 <= z)%Z ->
  Z.to_N (Wasm_int.Int32.unsigned (Wasm_int.Int32.repr z)) = (Z.to_N z mod i32mod)%N.
Proof.
  intros z Hz. rewrite Wasm_int.Int32.unsigned_repr_eq.
  rewrite (Z2N.inj_mod z Wasm_int.Int32.modulus Hz
             (ltac:(have := Wasm_int.Int32.modulus_pos; lia))).
  unfold i32mod. reflexivity.
Qed.

Lemma N_to_i32_mod : forall a, N_to_i32 (a mod i32mod)%N = N_to_i32 a.
Proof.
  intros a. unfold N_to_i32. apply Wasm_int.Int32.eqm_samerepr.
  unfold Wasm_int.Int32.eqm, Zbits.eqmod.
  exists (- (Z.of_N a / Wasm_int.Int32.modulus))%Z.
  rewrite N2Z.inj_mod Zof_i32mod.
  have Hp := Wasm_int.Int32.modulus_pos.
  have := Z.div_mod (Z.of_N a) Wasm_int.Int32.modulus (ltac:(lia)). lia.
Qed.

(* ---- app_binop / app_relop in N form ---- *)

Lemma app_binop_add : forall a b,
  app_binop (Binop_i BOI_add) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b))
  = Some (VAL_int32 (N_to_i32 (a + b)%N)).
Proof.
  intros a b.
  change (app_binop (Binop_i BOI_add) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b)))
    with (Some (VAL_int32 (Wasm_int.Int32.add (N_to_i32 a) (N_to_i32 b)))).
  do 2! f_equal. unfold Wasm_int.Int32.add, N_to_i32. rewrite N2Z.inj_add.
  apply Wasm_int.Int32.eqm_samerepr, Wasm_int.Int32.eqm_add;
    apply Wasm_int.Int32.eqm_unsigned_repr_l, Wasm_int.Int32.eqm_refl.
Qed.

Lemma app_binop_sub : forall a b, (b <= a)%N -> (a < i32mod)%N ->
  app_binop (Binop_i BOI_sub) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b))
  = Some (VAL_int32 (N_to_i32 (a - b)%N)).
Proof.
  intros a b H1 H2.
  change (app_binop (Binop_i BOI_sub) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b)))
    with (Some (VAL_int32 (Wasm_int.Int32.sub (N_to_i32 a) (N_to_i32 b)))).
  do 2! f_equal. apply N_to_i32_sub; assumption.
Qed.

Lemma app_binop_xor : forall c1 c2,
  app_binop (Binop_i BOI_xor) (VAL_int32 c1) (VAL_int32 c2)
  = Some (VAL_int32 (Wasm_int.Int32.xor c1 c2)).
Proof. reflexivity. Qed.

Lemma app_binop_and : forall c1 c2,
  app_binop (Binop_i BOI_and) (VAL_int32 c1) (VAL_int32 c2)
  = Some (VAL_int32 (Wasm_int.Int32.and c1 c2)).
Proof. reflexivity. Qed.

(** [alloc_ptr gmp align] is the pointer [$alloc] hands out: [gmp] rounded up
    to a multiple of [align].  Writing it as [align * (../align)] rather than
    as a bit pattern is what makes the two facts the callers need — that
    [align] divides it, and that it sits in [[gmp, gmp + align - 1]] —
    immediate. *)
Definition alloc_ptr (gmp align : N) : N :=
  (align * ((gmp + (align - 1)) / align))%N.

Arguments alloc_ptr : simpl never.

Lemma alloc_ptr_bounds : forall gmp align,
  (1 <= align)%N ->
  (gmp <= alloc_ptr gmp align <= gmp + (align - 1))%N /\
  (exists k, alloc_ptr gmp align = (align * k)%N).
Proof.
  intros gmp align Hal1.
  split; [| unfold alloc_ptr; eexists; reflexivity].
  unfold alloc_ptr.
  have Hdm := N.Div0.div_mod (gmp + (align - 1)) align.
  have Hmlt : ((gmp + (align - 1)) mod align < align)%N
    by (apply N.mod_lt; lia).
  set (X := (align * ((gmp + (align - 1)) / align))%N) in *.
  set (R := ((gmp + (align - 1)) mod align)%N) in *.
  lia.
Qed.

Lemma app_binop_align_up : forall gmp align,
  (align = 1 \/ align = 4 \/ align = 8)%N -> (gmp + align < i32mod)%N ->
  app_binop (Binop_i BOI_and)
    (VAL_int32 (N_to_i32 (gmp + (align - 1))%N))
    (VAL_int32 (Wasm_int.Int32.xor (N_to_i32 (align - 1)%N) Wasm_int.Int32.mone))
  = Some (VAL_int32 (N_to_i32 (alloc_ptr gmp align))).
Proof.
  intros gmp align Hal Hwrap. unfold alloc_ptr.
  assert (Hal1 : (1 <= align)%N) by (destruct Hal as [E|[E|E]]; subst align; lia).
  assert (Hx : (gmp + (align - 1) < i32mod)%N) by lia.
  rewrite (app_binop_and _ _). do 2! f_equal.
  rewrite and_xor_mone and_mask_mod; [| assumption | assumption ].
  rewrite N_to_i32_sub.
  - f_equal.
    have Hdm := N.Div0.div_mod (gmp + (align - 1)) align.
    have Hml := N.Div0.mod_le (gmp + (align - 1)) align. lia.
  - have := N.Div0.mod_le (gmp + (align - 1)) align. lia.
  - assumption.
Qed.

Lemma app_relop_gt_u : forall a b, (a < i32mod)%N -> (b < i32mod)%N ->
  app_relop (Relop_i (ROI_gt SX_U)) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b))
  = (b <? a)%N.
Proof.
  intros a b Ha Hb.
  change (app_relop (Relop_i (ROI_gt SX_U)) (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 b)))
    with (Wasm_int.Int32.ltu (N_to_i32 b) (N_to_i32 a)).
  unfold Wasm_int.Int32.ltu.
  rewrite (unsigned_N_to_i32 a Ha) (unsigned_N_to_i32 b Hb).
  destruct (Coqlib.zlt (Z.of_N b) (Z.of_N a)) as [Hl|Hl];
    destruct (b <? a)%N eqn:Hlt; try reflexivity.
  - apply N.ltb_ge in Hlt. lia.
  - apply N.ltb_lt in Hlt. lia.
Qed.

Lemma app_relop_eq_mone_false : forall a, (a + 1 < i32mod)%N ->
  app_relop (Relop_i ROI_eq) (VAL_int32 (N_to_i32 a)) (VAL_int32 Wasm_int.Int32.mone)
  = false.
Proof.
  intros a Ha.
  change (app_relop (Relop_i ROI_eq) (VAL_int32 (N_to_i32 a)) (VAL_int32 Wasm_int.Int32.mone))
    with (Wasm_int.Int32.eq (N_to_i32 a) Wasm_int.Int32.mone).
  unfold Wasm_int.Int32.eq.
  rewrite (unsigned_N_to_i32 a (ltac:(lia))) Wasm_int.Int32.unsigned_mone.
  rewrite i32mod_val in Ha.
  destruct (Coqlib.zeq (Z.of_N a) (Wasm_int.Int32.modulus - 1)%Z) as [Hl|Hl];
    [| reflexivity ].
  rewrite i32mod_Z in Hl. lia.
Qed.

Lemma app_relop_eq_mone_true :
  app_relop (Relop_i ROI_eq) (VAL_int32 Wasm_int.Int32.mone) (VAL_int32 Wasm_int.Int32.mone)
  = true.
Proof. apply Wasm_int.Int32.eq_true. Qed.

(* ---- shifts ---- *)

Lemma lt_i32mod_16 : (16 < i32mod)%N.
Proof. rewrite i32mod_val. lia. Qed.

Lemma shift_amount_16 :
  Wasm_int.Int32.unsigned
    (Wasm_int.Int32.repr
       (Wasm_int.Int32.unsigned (N_to_i32 16) mod Z.of_nat Wasm_int.Int32.wordsize))
  = 16%Z.
Proof.
  assert (Hw : (Wasm_int.Int32.unsigned (N_to_i32 16)
                mod Z.of_nat Wasm_int.Int32.wordsize)%Z = 16%Z).
  { rewrite (unsigned_N_to_i32 16 lt_i32mod_16). reflexivity. }
  rewrite Hw. apply Wasm_int.Int32.unsigned_repr.
  unfold Wasm_int.Int32.max_unsigned. rewrite i32mod_Z. lia.
Qed.

Lemma app_binop_shr_u_16 : forall a, (a < i32mod)%N ->
  app_binop (Binop_i (BOI_shr SX_U))
    (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 16))
  = Some (VAL_int32 (N_to_i32 (a / 65536)%N)).
Proof.
  intros a Ha.
  change (app_binop (Binop_i (BOI_shr SX_U))
            (VAL_int32 (N_to_i32 a)) (VAL_int32 (N_to_i32 16)))
    with (Some (VAL_int32 (Wasm_int.Int32.ishr_u (N_to_i32 a) (N_to_i32 16)))).
  do 2! f_equal.
  unfold Wasm_int.Int32.ishr_u, Wasm_int.Int32.shru.
  rewrite shift_amount_16 (unsigned_N_to_i32 a Ha).
  unfold N_to_i32. f_equal.
  rewrite Z.shiftr_div_pow2; [| lia ].
  change (2 ^ 16)%Z with 65536%Z. rewrite N2Z.inj_div. reflexivity.
Qed.

Section Inv.

Context `{ho : host}.

(** ** The invariant the adapter must preserve

    The fragment of the backend's [INV] that the adapter reads or writes.  The
    real [INV] (CertiRocq [LambdaANF_to_Wasm_correct.v]) is a far larger
    conjunction; the bridge lemma owed when CertiRocq becomes a dependency is
    that it implies this. *)
Record adapter_inv (sr : store_record) (inst : moduleinst) (gmp : N) : Prop := {
  (** the allocation pointer is where we think it is *)
  ai_gmp : sglob_val sr inst glob_mem_ptr = Some (N_to_value gmp);
  (** and it is even, so that a pointer is never mistaken for an immediate *)
  ai_gmp_even : exists k, gmp = (2 * k)%N;
  (** no allocation failure has been signalled *)
  ai_oom : sglob_val sr inst glob_out_of_mem = Some (N_to_value 0);
  (** the memory exists and covers everything allocated so far *)
  ai_mem : exists m, smem sr inst = Some m /\ (gmp <= mem_length m)%N;
  (** addresses stay in i32 range, with room for one more cell *)
  ai_no_wrap : (gmp + 12 < i32mod)%N;
  (** the globals the adapter writes are distinct cells of the store, so that
      setting one does not disturb another.  Instantiation gives this; it is
      needed because [sglob_val] goes through [inst_globals], which is in
      principle allowed to alias. *)
  ai_glob_distinct :
    sglob_ind sr inst glob_mem_ptr <> sglob_ind sr inst glob_out_of_mem
}.

(** [$alloc] cannot preserve [adapter_inv]: called with [align = 1] and an odd
    [n] — which is exactly how [$lower] allocates byte buffers — it leaves the
    allocation pointer odd, so [ai_gmp_even] fails.  Evenness is re-established
    by the caller, which knows the alignment it asked for; what survives
    [$alloc] unconditionally is this weaker invariant. *)
Record alloc_inv (sr : store_record) (inst : moduleinst) (gmp : N) : Prop := {
  aai_gmp : sglob_val sr inst glob_mem_ptr = Some (N_to_value gmp);
  aai_oom : sglob_val sr inst glob_out_of_mem = Some (N_to_value 0);
  aai_mem : exists m, smem sr inst = Some m /\ (gmp <= mem_length m)%N;
  aai_no_wrap : (gmp + 12 < i32mod)%N;
  aai_glob_distinct :
    sglob_ind sr inst glob_mem_ptr <> sglob_ind sr inst glob_out_of_mem
}.

Lemma adapter_inv_alloc : forall sr inst gmp,
  adapter_inv sr inst gmp -> alloc_inv sr inst gmp.
Proof.
  intros sr inst gmp H. constructor.
  - apply (ai_gmp _ _ _ H).
  - apply (ai_oom _ _ _ H).
  - apply (ai_mem _ _ _ H).
  - apply (ai_no_wrap _ _ _ H).
  - apply (ai_glob_distinct _ _ _ H).
Qed.

(** ** What the instance must look like

    The adapter's bodies call each other by index, so a theorem about them is
    a theorem about an instance in which those indices resolve correctly.
    These are the obligations that instantiation (or, after merging, the
    linker) has to discharge. *)
Definition native_at (sr : store_record) (inst : moduleinst)
                     (i : funcidx) (ty : function_type) (f : module_func) : Prop :=
  exists a, lookup_N (inst_funcs inst) i = Some a /\
            lookup_N sr.(s_funcs) a = Some (FC_func_native ty inst f).

Definition host_at (sr : store_record) (inst : moduleinst)
                   (i : funcidx) (ty : function_type) (h : host_function) : Prop :=
  exists a, lookup_N (inst_funcs inst) i = Some a /\
            lookup_N sr.(s_funcs) a = Some (FC_func_host ty h).
(** ** Reading a field

    [BI_load] takes its static offset as part of the [memarg], so the address
    [r_load_success] reads at is [addr] with offset [off]; [load_i32] in
    [echo_heap.v] reads at [addr + off] with offset 0.  The two agree. *)
Lemma load_word_offset : forall m addr off,
  load m addr off 4 = load m (addr + off)%N 0%N 4.
Proof.
  intros m addr off. unfold load.
  replace (addr + (off + 4))%N with (addr + off + (0 + 4))%N by lia.
  rewrite N.add_0_r. reflexivity.
Qed.

(** ** Writing a global, growing the memory

    [$alloc] is the only function in the adapter that changes the store, and
    it changes it in exactly two ways: [global.set] and [memory.grow].  These
    are the facts the proof needs about both, isolated here so that the
    stepping proof can stay about stepping. *)

Lemma supdate_glob_exists : forall sr inst j v w,
  sglob_val sr inst j = Some w ->
  exists sr', supdate_glob sr inst j v = Some sr'.
Proof.
  intros sr inst j v w H.
  unfold sglob_val, sglob, sglob_ind in H.
  destruct (lookup_N (inst_globals inst) j) as [a|] eqn:Ha; [|discriminate].
  destruct (lookup_N (s_globals sr) a) as [g|] eqn:Hg; [|discriminate].
  unfold supdate_glob, sglob_ind. rewrite Ha. cbn.
  unfold supdate_glob_s. rewrite Hg. cbn. eexists. reflexivity.
Qed.

Lemma supdate_glob_same : forall sr inst j v sr',
  supdate_glob sr inst j v = Some sr' ->
  sglob_val sr' inst j = Some v.
Proof.
  intros sr inst j v sr' H.
  unfold supdate_glob, sglob_ind in H.
  destruct (lookup_N (inst_globals inst) j) as [a|] eqn:Ha; [|discriminate].
  cbn in H. unfold supdate_glob_s in H.
  destruct (lookup_N (s_globals sr) a) as [g|] eqn:Hg; [|discriminate].
  cbn in H. injection H as <-.
  unfold sglob_val, sglob, sglob_ind. rewrite Ha. cbn.
  unfold lookup_N. rewrite nth_error_set_eq. reflexivity.
Qed.

Lemma supdate_glob_other : forall sr inst j j' v sr' w,
  sglob_ind sr inst j <> sglob_ind sr inst j' ->
  sglob_val sr inst j' = Some w ->
  supdate_glob sr inst j v = Some sr' ->
  sglob_val sr' inst j' = Some w.
Proof.
  intros sr inst j j' v sr' w Hne Hval H.
  unfold sglob_ind in Hne.
  unfold supdate_glob, sglob_ind in H.
  destruct (lookup_N (inst_globals inst) j) as [a|] eqn:Ha; [|discriminate].
  cbn in H. unfold supdate_glob_s in H.
  destruct (lookup_N (s_globals sr) a) as [g|] eqn:Hg; [|discriminate].
  cbn in H. injection H as <-.
  unfold sglob_val, sglob, sglob_ind in Hval |- *.
  destruct (lookup_N (inst_globals inst) j') as [a'|] eqn:Ha'; [|discriminate].
  cbn in Hval |- *.
  assert (Hane : N.to_nat a <> N.to_nat a').
  { intros Heq. assert (Haa : a = a') by lia. subst a'.
    apply Hne. rewrite ?Ha ?Ha'. reflexivity. }
  unfold lookup_N in Hval |- *.
  rewrite nth_error_set_neq; [exact Hval | exact Hane |].
  assert (Hlt : N.to_nat a < Datatypes.length (s_globals sr)).
  { apply List.nth_error_Some. unfold lookup_N in Hg. rewrite Hg. discriminate. }
  lias.
Qed.

Lemma supdate_glob_mem : forall sr inst j v sr',
  supdate_glob sr inst j v = Some sr' -> smem sr' inst = smem sr inst.
Proof.
  intros sr inst j v sr' H.
  unfold supdate_glob, sglob_ind in H.
  destruct (lookup_N (inst_globals inst) j) as [a|] eqn:Ha; [|discriminate].
  cbn in H. unfold supdate_glob_s in H.
  destruct (lookup_N (s_globals sr) a) as [g|] eqn:Hg; [|discriminate].
  cbn in H. injection H as <-. reflexivity.
Qed.

Lemma supdate_glob_funcs : forall sr inst j v sr',
  supdate_glob sr inst j v = Some sr' -> s_funcs sr' = s_funcs sr.
Proof.
  intros sr inst j v sr' H.
  unfold supdate_glob, sglob_ind in H.
  destruct (lookup_N (inst_globals inst) j) as [a|] eqn:Ha; [|discriminate].
  cbn in H. unfold supdate_glob_s in H.
  destruct (lookup_N (s_globals sr) a) as [g|] eqn:Hg; [|discriminate].
  cbn in H. injection H as <-. reflexivity.
Qed.

Lemma supdate_glob_ind : forall sr inst j v sr' j',
  supdate_glob sr inst j v = Some sr' ->
  sglob_ind sr' inst j' = sglob_ind sr inst j'.
Proof. reflexivity. Qed.

(** Storing a word, at the level of the store record.  [r_store_success]
    speaks of [smem_store]; everything else here speaks of [store_i32] on the
    memory instance, and this is the bridge.  A store touches [s_mems] only,
    so the globals and the function table survive it unchanged — which is what
    keeps [adapter_inv] and [adapter_instance] alive across one. *)
Lemma smem_store_spec : forall sr inst a off v m m',
  smem sr inst = Some m ->
  store_i32 m (a + off)%N v = Some m' ->
  exists sr',
    smem_store sr inst a off v T_i32 = Some sr' /\
    smem sr' inst = Some m' /\
    s_globals sr' = s_globals sr /\
    s_funcs sr' = s_funcs sr.
Proof.
  intros sr inst a off v m m' Hmem Hst.
  unfold store_i32 in Hst. rewrite <- store_offset in Hst.
  unfold smem in Hmem. unfold smem_store.
  destruct (lookup_N (inst_mems inst) 0%N) as [addr|] eqn:Haddr; [|discriminate].
  rewrite Hmem.
  (* the length [smem_store] passes is [tnum_length T_i32] under mathcomp's
     [nat_of_bin] coercion, which is [4] only up to conversion *)
  assert (Hst' : store m a off (serialise_num v)
                   (ssrnat.nat_of_bin (tnum_length T_i32)) = Some m')
    by exact Hst.
  rewrite Hst'.
  eexists. split; [reflexivity|]. split; [| split; reflexivity].
  unfold smem. rewrite Haddr. cbn. unfold lookup_N.
  rewrite nth_error_set_eq. reflexivity.
Qed.

(** Growing the memory keeps everything already allocated, which is the only
    thing the heap representation cares about: [mem_grow_lookup] says the bytes
    below the old length survive, and [mem_grow_length] says the new length is
    the old one plus the pages asked for. *)
Lemma mem_grow_agree : forall (mi mi' : meminst) k,
  mem_grow mi k = Some mi' ->
  (mem_length mi' = mem_length mi + k * page_size)%N /\
  (mem_size mi + k <= mem_limit_bound)%N /\
  mem_agree_below mi mi' (mem_length mi).
Proof.
  intros mi mi' k H. unfold mem_grow in H.
  destruct (N.leb (mem_size mi + k) mem_limit_bound) eqn:Hlim; [|discriminate].
  apply N.leb_le in Hlim.
  destruct (Wasm.memory.mem_grow (k * page_size) (meminst_data mi)) as [d|] eqn:Hd;
    [|discriminate].
  assert (Hlen : (Wasm.memory.mem_length d
                  = Wasm.memory.mem_length (meminst_data mi) + k * page_size)%N)
    by (eapply mem_grow_length; exact Hd).
  assert (Hdata : meminst_data mi' = d).
  { destruct (lim_max (meminst_type mi)) as [maxlim|].
    - destruct (N.leb (mem_size mi + k) maxlim) eqn:Hmax; [|discriminate].
      injection H as <-. reflexivity.
    - injection H as <-. reflexivity. }
  assert (Hlenm : (mem_length mi' = mem_length mi + k * page_size)%N)
    by (unfold mem_length; rewrite Hdata; exact Hlen).
  assert (Hlk : forall i, (i < mem_length mi)%N ->
            mem_lookup i d = mem_lookup i (meminst_data mi)).
  { intros i Hi. eapply mem_grow_lookup; [| exact Hd]. unfold mem_length in Hi. exact Hi. }
  split; [exact Hlenm | split; [exact Hlim |]].
  intros addr len Haddr.
  assert (Hb1 : N.leb (addr + (0 + len)) (mem_length mi') = true)
    by (apply N.leb_le; lia).
  assert (Hb2 : N.leb (addr + (0 + len)) (mem_length mi) = true)
    by (apply N.leb_le; lia).
  assert (Hrb : read_bytes mi' (addr + 0)%N (ssrnat.nat_of_bin len)
                = read_bytes mi (addr + 0)%N (ssrnat.nat_of_bin len)).
  { unfold read_bytes. rewrite Hdata. f_equal.
    apply List.map_ext_in. intros off Hoff. apply Hlk.
    apply In_iota in Hoff. rewrite nat_of_bin_N in Hoff. lia. }
  unfold load. rewrite Hb1 Hb2 Hrb. reflexivity.
Qed.

Lemma smem_grow_spec : forall sr inst k sr' sz mi,
  smem sr inst = Some mi ->
  smem_grow sr inst k = Some (sr', sz) ->
  exists mi', smem sr' inst = Some mi'
           /\ (mem_length mi' = mem_length mi + k * page_size)%N
           /\ (mem_size mi + k <= mem_limit_bound)%N
           /\ mem_agree_below mi mi' (mem_length mi)
           /\ sz = mem_size mi
           /\ s_globals sr' = s_globals sr
           /\ s_funcs sr' = s_funcs sr.
Proof.
  intros sr inst k sr' sz mi Hmem H.
  unfold smem in Hmem. unfold smem_grow in H.
  destruct (lookup_N (inst_mems inst) 0%N) as [addr|] eqn:Haddr; [|discriminate].
  rewrite Hmem in H.
  destruct (mem_grow mi k) as [mi'|] eqn:Hgrow; [|discriminate].
  injection H as <- <-.
  have [Hlen [Hlim Hagree]] := mem_grow_agree _ _ _ Hgrow.
  exists mi'. repeat split; try assumption; try reflexivity.
  unfold smem. rewrite Haddr. cbn. unfold lookup_N.
  rewrite nth_error_set_eq. reflexivity.
Qed.

(** [have := memory.size << 16] can wrap — a 4GiB memory reports zero free
    bytes — so the proof never computes it.  All it needs is that [have] never
    claims more memory than there is, which holds whatever the shift does:
    [x mod 2^32 <= x] and [(len / 64Ki) * 64Ki <= len]. *)
Lemma mem_size_shl : forall (mi : meminst), exists have,
  app_binop (Binop_i BOI_shl)
    (VAL_int32 (N_to_i32 (mem_size mi))) (VAL_int32 (N_to_i32 16))
  = Some (VAL_int32 (N_to_i32 have)) /\
  (have <= mem_length mi)%N /\ (have < i32mod)%N.
Proof.
  intros mi.
  exists (Z.to_N (Wasm_int.Int32.unsigned
            (Wasm_int.Int32.ishl (N_to_i32 (mem_size mi)) (N_to_i32 16)))).
  split; [| split; [| apply i32_as_N_lt ]].
  - change (app_binop (Binop_i BOI_shl)
              (VAL_int32 (N_to_i32 (mem_size mi))) (VAL_int32 (N_to_i32 16)))
      with (Some (VAL_int32
              (Wasm_int.Int32.ishl (N_to_i32 (mem_size mi)) (N_to_i32 16)))).
    do 2! f_equal. apply i32_as_N.
  - unfold Wasm_int.Int32.ishl, Wasm_int.Int32.shl.
    rewrite shift_amount_16.
    have Hnn := Wasm_int.Int32.unsigned_range (N_to_i32 (mem_size mi)).
    rewrite Z.shiftl_mul_pow2; [| lia ].
    change (2 ^ 16)%Z with 65536%Z.
    rewrite to_N_unsigned_repr; [| lia ].
    rewrite Z2N.inj_mul; [| lia | lia ].
    have Hu : (Z.to_N (Wasm_int.Int32.unsigned (N_to_i32 (mem_size mi)))
               <= mem_size mi)%N.
    { unfold N_to_i32. rewrite to_N_unsigned_repr; [| lia ].
      rewrite N2Z.id. apply N.Div0.mod_le. }
    have Hml := N.Div0.mod_le
      (Z.to_N (Wasm_int.Int32.unsigned (N_to_i32 (mem_size mi))) * Z.to_N 65536)%N
      i32mod.
    have Hps : page_size = 65536%N. { reflexivity. }
    have Hds : (mem_size mi * page_size <= mem_length mi)%N.
    { unfold mem_size. rewrite N.mul_comm. apply N.Div0.mul_div_le. }
    rewrite Hps in Hds.
    change (Z.to_N 65536) with 65536%N in *. lia.
Qed.
Lemma sglob_val_globals : forall sr sr' inst j,
  s_globals sr' = s_globals sr -> sglob_val sr' inst j = sglob_val sr inst j.
Proof.
  intros sr sr' inst j H. unfold sglob_val, sglob, sglob_ind. rewrite H. reflexivity.
Qed.

Lemma mem_agree_below_le : forall mi mi' b b',
  mem_agree_below mi mi' b -> (b' <= b)%N -> mem_agree_below mi mi' b'.
Proof. intros mi mi' b b' H Hle. eapply mem_agree_below_weaken; eauto. Qed.
(** [rs_if_false] / [rs_if_true] want the condition as a concrete [i32]; what
    the adapter leaves on the stack is [wasm_bool] of a comparison.  Taking the
    comparison's value as a side goal rather than rewriting it into the
    configuration keeps the proof out of the business of matching the exact
    shape [cbn] left the [i32] constants in. *)
Lemma rs_if_false_bool : forall (b : bool) tb es1 es2,
  b = false ->
  reduce_simple
    [:: AI_basic (BI_const_num (VAL_int32 (wasm_bool b)));
        AI_basic (BI_if tb es1 es2)]
    [:: AI_basic (BI_block tb es2)].
Proof. intros b tb es1 es2 Hb. subst b. apply rs_if_false. reflexivity. Qed.

Lemma rs_if_true_bool : forall (b : bool) tb es1 es2,
  b = true ->
  reduce_simple
    [:: AI_basic (BI_const_num (VAL_int32 (wasm_bool b)));
        AI_basic (BI_if tb es1 es2)]
    [:: AI_basic (BI_block tb es1)].
Proof.
  intros b tb es1 es2 Hb. subst b. apply rs_if_true.
  exact Wasm_int.Int32.one_not_zero.
Qed.

End Inv.
