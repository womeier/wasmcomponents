(** * The adapter code [componentize-formal] emits, and its vocabulary

    Everything an adapter's correctness proof needs that does not mention
    the world's WIT types.  The split is the one the generator itself
    makes: the instruction shorthands and the fragments below are emitted
    verbatim for every world (only indices and sizes move), whereas
    lowering and lifting a particular type into the canonical ABI is
    generated per type and proven per type, one layer up.

    ** The scratch area

    Every generated export starts the same way: round [glob_mem_ptr] up to
    a multiple of 8, take a fixed number of bytes for the canonical return
    area plus whatever the arguments need boxed, and leave [glob_mem_ptr]
    past them.  [scratch_ok] below is that layout as a proposition —
    [alloc_scratch] establishes it, the lowering code consumes it, and the
    disjointness facts an adapter's stores need are its consequences
    rather than five unexplained inequalities at each call site. *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics memory opsem host
  properties.

From Stdlib Require Import ZArith BinNat NArith.Nnat List Lia
  Relations.Relations Relations.Relation_Operators.

From WasmGlue Require Import wasm_steps wasm_mem certirocq_heap certirocq_inv.

Import ssreflect ssrbool eqtype.
Import ListNotations.
Import seq.

(** ** Shorthands

    These keep an emitted body diffable against its [.wat] by eye, which is
    the only check there is until the binary parser closes the loop. *)
Definition i32c (n : N) : basic_instruction :=
  BI_const_num (VAL_int32 (N_to_i32 n)).

Definition i64c (z : Z) : basic_instruction :=
  BI_const_num (VAL_int64 (Z_to_i64 z)).

(** [i32.const -8], the mask that rounds an address down to a multiple of 8. *)
Definition i32c_neg8 : basic_instruction :=
  BI_const_num (VAL_int32 (Wasm_int.Int32.repr (-8)%Z)).

Definition add    : basic_instruction := BI_binop T_i32 (Binop_i BOI_add).
Definition and_   : basic_instruction := BI_binop T_i32 (Binop_i BOI_and).
Definition mul    : basic_instruction := BI_binop T_i32 (Binop_i BOI_mul).
Definition gt_u   : basic_instruction := BI_relop T_i32 (Relop_i (ROI_gt SX_U)).
Definition lt_s64 : basic_instruction := BI_relop T_i64 (Relop_i (ROI_lt SX_S)).

(** Loads and stores at their natural alignment, which is what [wasm-tools]
    encodes when the WAT leaves [align=] off: [align] is the log2 of the
    width in bytes. *)
Definition load_word (off : N) : basic_instruction :=
  BI_load T_i32 None {| memarg_offset := off; memarg_align := 2%N |}.

Definition load_i64_at (off : N) : basic_instruction :=
  BI_load T_i64 None {| memarg_offset := off; memarg_align := 3%N |}.

Definition store_byte (off : N) : basic_instruction :=
  BI_store T_i32 (Some Tp_i8) {| memarg_offset := off; memarg_align := 0%N |}.

Definition store_i64_at (off : N) : basic_instruction :=
  BI_store T_i64 None {| memarg_offset := off; memarg_align := 3%N |}.

(** The backend emits one table and the adapter imports it; imports come
    first and an adapter defines none of its own, so it is index 0. *)
Definition tbl_funcs : tableidx := 0%N.

(** Page size, as the generator spells it. *)
Definition page_bytes : N := 65536.

(** ** The fragments every export body is built from

    Parameterised by the local slots the generator assigned and by the
    sizes it computed, which is all that varies between worlds. *)

(** Carve out [scratch] bytes at an 8-aligned [glob_mem_ptr]: [l_ret] gets
    the base — the canonical return area — and [l_arg] the first byte past
    it, where the lowered arguments go.  Runs before the backend's
    [$main_function], so the heap is still empty. *)
Definition alloc_scratch (l_ret l_arg : localidx) (ret_size scratch : N) : expr :=
  [ BI_global_get glob_mem_ptr ; i32c 7 ; add ; i32c_neg8 ; and_
  ; BI_global_set glob_mem_ptr
  ; BI_global_get glob_mem_ptr ; BI_local_set l_ret
  ; BI_local_get l_ret ; i32c ret_size ; add ; BI_local_set l_arg
  ; BI_local_get l_ret ; i32c scratch ; add ; BI_global_set glob_mem_ptr ].

(** Nothing bounds-checks what the adapter takes for itself, and a store
    past the end of the memory would trap instead of answering.  Reaching
    the [unreachable] means there is no room for the return area at the
    bottom of a fresh heap, which cannot be reported through a return area
    that does not fit — the one failure with nowhere to go. *)
Definition room_check : expr :=
  [ BI_global_get glob_mem_ptr ; BI_memory_size ; i32c page_bytes ; mul ; gt_u
  ; BI_if (BT_valtype None) [ BI_unreachable ] [] ].

(** Answering the error arm and returning, as every failure path does:
    [err_fn] writes [case] into the return area, whose address is then the
    export's result. *)
Definition fail_with (l_ret : localidx) (err_fn : funcidx) (case : N) : expr :=
  [ BI_local_get l_ret ; i32c case ; BI_call err_fn
  ; BI_local_get l_ret ; BI_return ].

(** [glob_out_of_mem] means [glob_result] was never written, so this runs
    after every call into the backend. *)
Definition oom_guard (l_ret : localidx) (err_fn : funcidx) (case : N) : expr :=
  [ BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) (fail_with l_ret err_fn case) [] ].

(** ** The scratch area, as a proposition

    [ret] is the return area's base and [arg] the first byte past it; the
    area runs to [ret + scratch], and [gmp] is the allocation frontier the
    backend will allocate above.  One record rather than the handful of
    inequalities each lowering step needs, which are derived below. *)
Record scratch_ok (ret_size scratch ret arg gmp : N) : Prop := {
  (** the arguments go directly above the return area *)
  sk_arg  : arg = (ret + ret_size)%N;
  (** with room for at least one 8-byte box *)
  sk_room : (ret_size + 8 <= scratch)%N;
  (** and the whole area is below what the backend has handed out *)
  sk_fits : (ret + scratch <= gmp)%N;
  (** CertiRocq's heap invariant wants even addresses *)
  sk_even : (exists k, arg = 2 * k)%N;
  (** and every address in it is an [i32] *)
  sk_mod  : (gmp < i32mod)%N
}.

(** A box written at [arg] stays below the frontier, so it cannot collide
    with anything the backend allocated. *)
Lemma sk_box_below : forall rs sc ret arg gmp,
  scratch_ok rs sc ret arg gmp -> (arg + 8 <= gmp)%N.
Proof.
  intros rs sc ret arg gmp [Harg Hroom Hfits _ _]. lia.
Qed.

(** ... and above the return area, so writing it cannot disturb the answer. *)
Lemma sk_ret_below : forall rs sc ret arg gmp,
  scratch_ok rs sc ret arg gmp -> (ret + rs <= arg)%N.
Proof.
  intros rs sc ret arg gmp [Harg _ _ _ _]. lia.
Qed.

Lemma sk_arg_mod : forall rs sc ret arg gmp,
  scratch_ok rs sc ret arg gmp -> (arg < i32mod)%N.
Proof.
  intros rs sc ret arg gmp [Harg Hroom Hfits _ Hmod]. lia.
Qed.

Lemma sk_ret_mod : forall rs sc ret arg gmp,
  scratch_ok rs sc ret arg gmp -> (ret < i32mod)%N.
Proof.
  intros rs sc ret arg gmp [Harg Hroom Hfits _ Hmod]. lia.
Qed.

(** The frontier only ever moves up, and the layout does not care how far. *)
Lemma scratch_ok_gmp_mono : forall rs sc ret arg gmp gmp',
  scratch_ok rs sc ret arg gmp ->
  (gmp <= gmp')%N ->
  (gmp' < i32mod)%N ->
  scratch_ok rs sc ret arg gmp'.
Proof.
  intros rs sc ret arg gmp gmp' [Harg Hroom Hfits Heven Hmod] Hle Hmod'.
  constructor; try lia; exact Heven.
Qed.

(** ** i32/i64 arithmetic the fragments need

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

(** [alloc_scratch]'s rounding of the allocation pointer, as one binop. *)
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

(** A range check on a [u64] reads it as a signed [i64]: negative means the
    top bit is set, i.e. the value is 2^63 or more. *)

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

    Canonical-ABI discriminants are bytes, written with [i32.store8] and
    read back with [load_u8].  [store_packed] is [store] at length 1, which
    keeps only the low byte of the operand — little-endian, by the vendored
    compcert's [Archi]. *)

Lemma bytes_takefill_takefill : forall b l (bs : bytes),
  bytes_takefill b l (bytes_takefill b l bs) = bytes_takefill b l bs.
Proof.
  intros b l bs. apply bytes_takefill_id. apply bytes_takefill_length.
Qed.

Section WithMemory.

Context `{BlockUpdateMemory}.

Lemma store_takefill : forall (m : meminst) (a off : N) (bs : bytes) (l : nat),
  store m a off (bytes_takefill #00 l bs) l = store m a off bs l.
Proof.
  intros m a off bs l. unfold store.
  destruct (N.leb (a + off + N.of_nat l) (mem_length m)); [| reflexivity].
  rewrite bytes_takefill_takefill. reflexivity.
Qed.

(** The two discriminant values a [result] ever stores. *)
Lemma takefill_serialise_0 :
  bytes_takefill #00 1 (serialise_i32 (N_to_i32 0)) = [:: Integers.Byte.zero].
Proof. vm_compute. reflexivity. Qed.

Lemma takefill_serialise_1 :
  bytes_takefill #00 1 (serialise_i32 (N_to_i32 1)) = [:: Integers.Byte.one].
Proof. vm_compute. reflexivity. Qed.

(** Writing the low byte of [N_to_i32 k] and reading it back.  Stated for 0
    and 1 only because the general statement needs [k mod 256] and no
    discriminant in sight exceeds a bool. *)
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

(** And conversely for word reads below a byte store. *)
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

Section Adapter.

Context `{ho : host}.

(** ** The store-record level of the stores an adapter does

    Like [smem_store_spec] in [certirocq_inv.v], with two additions: the
    packed (byte) width, and the [s_tables] clause — lowering an argument
    happens after the table entry for the closure is known, and the entry
    has to survive it. *)

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

(** [native_at] only reads [inst_funcs] and the store's function list, so it
    survives everything an adapter does to the store. *)
Lemma native_at_funcs : forall sr sr' inst i ty fn,
  s_funcs sr' = s_funcs sr ->
  native_at sr inst i ty fn ->
  native_at sr' inst i ty fn.
Proof.
  intros sr sr' inst i ty fn Hf [a [Hidx Hfunc]].
  exists a. split; [exact Hidx |]. rewrite Hf. exact Hfunc.
Qed.

(** ** Dispatching an [if] on a flag that is literally 0 or 1

    [glob_out_of_mem] is a flag, not a comparison, so neither
    [rs_if_*_bool] applies to it. *)
Lemma rs_if_false_N0 : forall tb es1 es2,
  reduce_simple
    [:: AI_basic (BI_const_num (VAL_int32 (N_to_i32 0)));
        AI_basic (BI_if tb es1 es2)]
    [:: AI_basic (BI_block tb es2)].
Proof. intros tb es1 es2. apply rs_if_false. reflexivity. Qed.

Lemma rs_if_true_N1 : forall tb es1 es2,
  reduce_simple
    [:: AI_basic (BI_const_num (VAL_int32 (N_to_i32 1)));
        AI_basic (BI_if tb es1 es2)]
    [:: AI_basic (BI_block tb es1)].
Proof.
  intros tb es1 es2. apply rs_if_true.
  change (N_to_i32 1) with Wasm_int.Int32.one. exact Wasm_int.Int32.one_not_zero.
Qed.

(** ** [oom_guard], on the arm where the backend did not signal

    The failing arm [return]s, so it is stated per world at the whole-frame
    level; falling through is world-independent. *)
Lemma oom_guard_pass : forall l_ret err_fn case hs sr fr rest,
  sglob_val sr (f_inst fr) glob_out_of_mem = Some (N_to_value 0) ->
  reduce_trans (hs, sr, fr, to_e_list (oom_guard l_ret err_fn case) ++ rest)
               (hs, sr, fr, rest).
Proof.
  intros l_ret err_fn case hs sr fr rest Hoom.
  cbn.
  dostep_nary 0. apply r_global_get. exact Hoom.
  dostep_nary 1. apply r_simple. apply rs_if_false_N0.
  dostep_nary 0.
  eapply r_block with (vs := [::]) (n := 0) (m := 0) (t1s := [::]) (t2s := [::]);
    reflexivity.
  dostep_nary 0. apply r_simple. apply rs_label_const. reflexivity.
  apply rt_refl.
Qed.

End Adapter.
