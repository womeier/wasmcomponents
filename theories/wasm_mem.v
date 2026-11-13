(** * Writing memory

    [$alloc] only ever moves the allocation pointer and grows the memory, so
    [echo_correct.v] gets away with [mem_grow_agree] alone.  The three
    functions that remain — [$some], [$lift_bytes] and [$lower] — all work by
    *storing* into the space [$alloc] handed them, and WasmCert specifies
    [store] by its definition only.

    This file supplies the three facts a caller of [store] needs, once, at the
    level of [store] and [load] rather than at the level of [load_i32], so
    that the word stores of [$some] and [$lift_bytes] and the byte stores of
    [$lower] are both instances:

    - [store_exists]       — a store that fits in the memory succeeds;
    - [load_store_same]    — it is visible where it was written;
    - [load_store_disjoint]— it is invisible everywhere else.

    Everything is derived from the [Memory] typeclass axioms, so it holds for
    whichever backing representation ([memory_list], [memory_vec]) the
    instantiation picks.  Nothing here mentions the echo slice; like
    [echo_steps.v] this is a gap in WasmCert that happens to be filled here.

    ** A note on [nat] versus [N]

    [load] takes its length as an [N] and passes it to [read_bytes], which
    takes a [nat] — the conversion is mathcomp's [nat_of_bin] coercion, which
    is silent in the source and therefore easy to lose a proof in.  Every
    statement below takes the length as a [nat] and writes [N.of_nat] at the
    [load] call, and [nat_of_bin_N] is the one lemma that crosses over. *)

From compcert Require Import lib.Integers common.Memdata.

From Wasm Require Import datatypes operations numerics memory bytes list_extra.

From Stdlib Require Import ZArith BinNat BinPos NArith.Nnat PArith.Pnat PeanoNat List Lia.

Import ssreflect ssrbool eqtype.

(** ** Crossing between mathcomp's [nat_of_bin] and [N.to_nat]

    [read_bytes]'s length argument is a [nat] and [load]'s is an [N]; the
    coercion that joins them is mathcomp's, and nothing in [Stdlib] knows
    about it. *)
Lemma nat_of_bin_N : forall n : N, ssrnat.nat_of_bin n = N.to_nat n.
Proof.
  case => //= p.
  elim: p => [p IH|p IH|] //=;
    rewrite ssrnat.natTrecE -?ssrnat.addnn -?ssrnat.plusE IH;
    [rewrite Pos2Nat.inj_xI | rewrite Pos2Nat.inj_xO]; lia.
Qed.

(** ** Lists

    [read_bytes] is [those] of a [map] over an [iota]; these two lemmas are
    what turn that into a statement about [nth_error]. *)
Lemma In_iota : forall (len start off : nat),
  List.In off (seq.iota start len) -> (start <= off)%nat /\ (off < start + len)%nat.
Proof.
  induction len as [| len IH]; intros start off Hin.
  - cbn in Hin. contradiction.
  - cbn in Hin. destruct Hin as [Heq | Hin]; [subst off; lia|].
    apply IH in Hin. lia.
Qed.

Lemma those0_map_iota : forall (A : Type) (g : nat -> option A) (res : list A) (start : nat),
  (forall k, (k < List.length res)%nat -> g (start + k)%nat = List.nth_error res k) ->
  those0 (List.map g (seq.iota start (List.length res))) = Some res.
Proof.
  intros A g res. induction res as [| r res' IH]; intros start Hg.
  - reflexivity.
  - assert (Hg0 : g start = Some r).
    { assert (H0 : g (start + 0)%nat = List.nth_error (r :: res') 0)
        by (apply Hg; cbn [List.length]; lia).
      rewrite Nat.add_0_r in H0. exact H0. }
    assert (HI : those0 (List.map g (seq.iota (S start) (List.length res'))) = Some res').
    { apply IH. intros k Hk.
      replace (S start + k)%nat with (start + S k)%nat by lia.
      apply (Hg (S k)). cbn [List.length]. lia. }
    cbn [List.length seq.iota List.map].
    rewrite Hg0. cbn [those0]. rewrite HI. reflexivity.
Qed.

(** [bytes_takefill] is the identity when the bytes already have the length
    the store asks for, which is the only case the adapter produces. *)
Lemma bytes_takefill_id : forall b l (bs : bytes),
  List.length bs = l -> bytes_takefill b l bs = bs.
Proof.
  intros b l bs Hlen. unfold bytes_takefill. subst l.
  rewrite Nat.ltb_irrefl. apply List.firstn_all.
Qed.

Lemma bytes_takefill_length : forall b l (bs : bytes),
  List.length (bytes_takefill b l bs) = l.
Proof.
  intros b l bs. unfold bytes_takefill.
  destruct (Nat.ltb (List.length bs) l) eqn:Hlt.
  - apply Nat.ltb_lt in Hlt.
    rewrite List.length_app List.repeat_length -ssrnat.minusE. lia.
  - apply Nat.ltb_ge in Hlt. apply List.firstn_length_le. lia.
Qed.

(** ** Serialisation

    [store_i32] writes [serialise_num] and [load_i32] reads it back with
    [wasm_deserialise]; that the two are inverse is the only fact about the
    byte encoding this development needs. *)
Lemma serialise_i32_length : forall c, List.length (serialise_i32 c) = 4%nat.
Proof. intros c. apply encode_int_length. Qed.

Lemma deserialise_serialise_i32 : forall c,
  wasm_deserialise (serialise_i32 c) T_i32 = VAL_int32 c.
Proof.
  intros c. unfold wasm_deserialise, serialise_i32.
  rewrite decode_encode_int. f_equal.
  change (two_p (Z.of_nat 4 * 8))%Z with Wasm_int.Int32.modulus.
  rewrite Z.mod_small; [apply Wasm_int.Int32.repr_unsigned |].
  apply Wasm_int.Int32.unsigned_range.
Qed.

(** The same two facts at [i64] width, which is what a boxed CertiRocq
    primitive integer ([certirocq_heap.v]'s [repr_prim]) is stored as. *)
Lemma serialise_i64_length : forall c, List.length (serialise_i64 c) = 8%nat.
Proof. intros c. apply encode_int_length. Qed.

Lemma deserialise_serialise_i64 : forall c,
  wasm_deserialise (serialise_i64 c) T_i64 = VAL_int64 c.
Proof.
  intros c. unfold wasm_deserialise, serialise_i64.
  rewrite decode_encode_int. f_equal.
  change (two_p (Z.of_nat 8 * 8))%Z with Wasm_int.Int64.modulus.
  rewrite Z.mod_small; [apply Wasm_int.Int64.repr_unsigned |].
  apply Wasm_int.Int64.unsigned_range.
Qed.

Section WithMemory.

Context `{Memory}.

(** ** [write_bytes], on the raw memory

    The four facts, by induction on the bytes written: it succeeds inside the
    memory, it does not change the length, it leaves everything outside the
    written range alone, and inside the range it holds what was written. *)
Lemma write_bytes_exists : forall (bs : bytes) (md : mem_t) (a : N),
  (a + N.of_nat (List.length bs) <= memory.mem_length md)%N ->
  exists md', write_bytes md a bs = Some md'.
Proof.
  induction bs as [| b bs IH]; intros md a Hlen.
  - exists md. reflexivity.
  - cbn [List.length] in Hlen. cbn [write_bytes].
    destruct (mem_update a b md) as [md1|] eqn:Hupd.
    + apply IH.
      assert (Hl : memory.mem_length md1 = memory.mem_length md)
        by (eapply mem_update_length; exact Hupd).
      rewrite Hl. lia.
    + exfalso. eapply mem_update_ib; [| exact Hupd]. lia.
Qed.

Lemma write_bytes_length : forall (bs : bytes) (md md' : mem_t) (a : N),
  write_bytes md a bs = Some md' ->
  memory.mem_length md' = memory.mem_length md.
Proof.
  induction bs as [| b bs IH]; intros md md' a Hw.
  - cbn in Hw. injection Hw as <-. reflexivity.
  - cbn in Hw. destruct (mem_update a b md) as [md1|] eqn:Hupd; [|discriminate].
    rewrite (IH _ _ _ Hw). eapply mem_update_length; exact Hupd.
Qed.

Lemma write_bytes_lookup_ne : forall (bs : bytes) (md md' : mem_t) (a i : N),
  write_bytes md a bs = Some md' ->
  ((i < a)%N \/ (a + N.of_nat (List.length bs) <= i)%N) ->
  mem_lookup i md' = mem_lookup i md.
Proof.
  induction bs as [| b bs IH]; intros md md' a i Hw Hdis.
  - cbn in Hw. injection Hw as <-. reflexivity.
  - cbn in Hw. destruct (mem_update a b md) as [md1|] eqn:Hupd; [|discriminate].
    cbn [List.length] in Hdis.
    assert (Hstep : mem_lookup i md' = mem_lookup i md1).
    { apply (IH md1 md' (a + 1)%N i Hw). lia. }
    rewrite Hstep. eapply mem_update_lookup_ne; [| exact Hupd]. lia.
Qed.

Lemma write_bytes_lookup_in : forall (bs : bytes) (md md' : mem_t) (a : N) (k : nat),
  write_bytes md a bs = Some md' ->
  (k < List.length bs)%nat ->
  mem_lookup (a + N.of_nat k)%N md' = List.nth_error bs k.
Proof.
  induction bs as [| b bs IH]; intros md md' a k Hw Hk.
  - cbn [List.length] in Hk. lia.
  - cbn in Hw. destruct (mem_update a b md) as [md1|] eqn:Hupd; [|discriminate].
    destruct k as [| k'].
    + replace (a + N.of_nat 0)%N with a by lia.
      assert (Hstep : mem_lookup a md' = mem_lookup a md1).
      { apply (write_bytes_lookup_ne bs md1 md' (a + 1)%N a Hw). lia. }
      rewrite Hstep. cbn [List.nth_error].
      eapply mem_update_lookup; exact Hupd.
    + replace (a + N.of_nat (S k'))%N with ((a + 1) + N.of_nat k')%N by lia.
      cbn [List.nth_error].
      apply (IH md1 md' (a + 1)%N k' Hw). cbn [List.length] in Hk. lia.
Qed.

(** ** [read_bytes], on a memory instance *)
Lemma read_bytes_spec : forall (m : meminst) (a : N) (bs : bytes),
  (forall k, (k < List.length bs)%nat ->
     mem_lookup (a + N.of_nat k)%N (meminst_data m) = List.nth_error bs k) ->
  read_bytes m a (List.length bs) = Some bs.
Proof.
  intros m a bs Hlk. unfold read_bytes.
  rewrite <- those_those0.
  apply (those0_map_iota _ _ bs 0%nat).
  intros k Hk. rewrite Nat.add_0_l. apply Hlk. exact Hk.
Qed.

Lemma read_bytes_ext : forall (m m' : meminst) (a : N) (len : nat),
  (forall k, (k < len)%nat ->
     mem_lookup (a + N.of_nat k)%N (meminst_data m')
     = mem_lookup (a + N.of_nat k)%N (meminst_data m)) ->
  read_bytes m' a len = read_bytes m a len.
Proof.
  intros m m' a len Hag. unfold read_bytes. f_equal.
  apply List.map_ext_in. intros off Hoff.
  apply In_iota in Hoff. apply Hag. lia.
Qed.

(** ** [store], on a memory instance *)
Lemma store_bound : forall (m m' : meminst) (a off : N) (bs : bytes) (l : nat),
  store m a off bs l = Some m' ->
  (a + off + N.of_nat l <= operations.mem_length m)%N.
Proof.
  intros m m' a off bs l Hs. unfold store in Hs.
  destruct (N.leb (a + off + N.of_nat l) (operations.mem_length m)) eqn:Hb;
    [| discriminate].
  apply N.leb_le. exact Hb.
Qed.

(** A static offset in a [memarg] is the same thing as adding to the dynamic
    address, which is how the [memarg]s of [echo_adapter.v] meet the
    offset-free lemmas below. *)
Lemma store_offset : forall (m : meminst) (a off : N) (bs : bytes) (l : nat),
  store m a off bs l = store m (a + off)%N 0%N bs l.
Proof. intros m a off bs l. unfold store. rewrite !N.add_0_r. reflexivity. Qed.

Lemma store_exists : forall (m : meminst) (a : N) (bs : bytes) (l : nat),
  (a + N.of_nat l <= operations.mem_length m)%N ->
  exists m', store m a 0%N bs l = Some m'.
Proof.
  intros m a bs l Hle. unfold store.
  assert (Hb : (a + 0 + N.of_nat l <=? operations.mem_length m)%N = true)
    by (apply N.leb_le; lia).
  rewrite Hb. unfold write_bytes_meminst.
  assert (Hbound : ((a + 0) + N.of_nat (List.length
                      (bytes_takefill Integers.Byte.zero l bs))
                    <= memory.mem_length (meminst_data m))%N).
  { rewrite bytes_takefill_length. unfold operations.mem_length in Hle. lia. }
  destruct (write_bytes_exists _ _ _ Hbound) as [md' Hw].
  rewrite Hw. eexists. reflexivity.
Qed.

Lemma store_length : forall (m m' : meminst) (a off : N) (bs : bytes) (l : nat),
  store m a off bs l = Some m' ->
  operations.mem_length m' = operations.mem_length m.
Proof.
  intros m m' a off bs l Hs. unfold store in Hs.
  destruct (N.leb (a + off + N.of_nat l) (operations.mem_length m)) eqn:Hb;
    [| discriminate].
  unfold write_bytes_meminst in Hs.
  destruct (write_bytes (meminst_data m) (a + off)%N
              (bytes_takefill Integers.Byte.zero l bs)) as [md'|] eqn:Hw;
    [| discriminate].
  injection Hs as <-. unfold operations.mem_length. cbn.
  eapply write_bytes_length; exact Hw.
Qed.

Lemma store_lookup_ne : forall (m m' : meminst) (a : N) (bs : bytes) (l : nat) (i : N),
  store m a 0%N bs l = Some m' ->
  ((i < a)%N \/ (a + N.of_nat l <= i)%N) ->
  mem_lookup i (meminst_data m') = mem_lookup i (meminst_data m).
Proof.
  intros m m' a bs l i Hs Hdis. unfold store in Hs.
  destruct (N.leb (a + 0 + N.of_nat l) (operations.mem_length m)) eqn:Hb;
    [| discriminate].
  unfold write_bytes_meminst in Hs.
  destruct (write_bytes (meminst_data m) (a + 0)%N
              (bytes_takefill Integers.Byte.zero l bs)) as [md'|] eqn:Hw;
    [| discriminate].
  injection Hs as <-. cbn.
  apply (write_bytes_lookup_ne _ _ _ (a + 0)%N i Hw).
  rewrite bytes_takefill_length. lia.
Qed.

Lemma store_lookup_in : forall (m m' : meminst) (a : N) (bs : bytes) (l k : nat),
  store m a 0%N bs l = Some m' ->
  List.length bs = l ->
  (k < l)%nat ->
  mem_lookup (a + N.of_nat k)%N (meminst_data m') = List.nth_error bs k.
Proof.
  intros m m' a bs l k Hs Hlen Hk. unfold store in Hs.
  destruct (N.leb (a + 0 + N.of_nat l) (operations.mem_length m)) eqn:Hb;
    [| discriminate].
  unfold write_bytes_meminst in Hs.
  rewrite (bytes_takefill_id _ _ _ Hlen) in Hs.
  destruct (write_bytes (meminst_data m) (a + 0)%N bs) as [md'|] eqn:Hw;
    [| discriminate].
  injection Hs as <-. cbn.
  replace (a + N.of_nat k)%N with ((a + 0) + N.of_nat k)%N by lia.
  apply (write_bytes_lookup_in _ _ _ _ _ Hw). lia.
Qed.

(** *** What a caller sees

    The two halves of a store's effect.  Both are stated with the length as a
    [nat], so that the [N.of_nat] at the [load] is the only place the two
    number types meet. *)
Lemma load_store_same : forall (m m' : meminst) (a : N) (bs : bytes) (l : nat),
  store m a 0%N bs l = Some m' ->
  List.length bs = l ->
  load m' a 0%N (N.of_nat l) = Some bs.
Proof.
  intros m m' a bs l Hs Hlen.
  have Hbd := store_bound _ _ _ _ _ _ Hs.
  have Hml := store_length _ _ _ _ _ _ Hs.
  unfold load.
  assert (Hb : (a + (0 + N.of_nat l) <=? operations.mem_length m')%N = true)
    by (apply N.leb_le; lia).
  rewrite Hb nat_of_bin_N Nat2N.id N.add_0_r.
  rewrite <- Hlen. apply read_bytes_spec.
  intros k Hk. eapply store_lookup_in; [exact Hs | exact Hlen | lia].
Qed.

Lemma load_store_disjoint :
  forall (m m' : meminst) (a : N) (bs : bytes) (l : nat) (b : N) (lb : nat),
  store m a 0%N bs l = Some m' ->
  ((b + N.of_nat lb <= a)%N \/ (a + N.of_nat l <= b)%N) ->
  load m' b 0%N (N.of_nat lb) = load m b 0%N (N.of_nat lb).
Proof.
  intros m m' a bs l b lb Hs Hdis.
  have Hml := store_length _ _ _ _ _ _ Hs.
  unfold load. rewrite Hml.
  destruct (N.leb (b + (0 + N.of_nat lb)) (operations.mem_length m)) eqn:Hb;
    [| reflexivity].
  rewrite nat_of_bin_N Nat2N.id N.add_0_r.
  apply read_bytes_ext. intros k Hk.
  eapply store_lookup_ne; [exact Hs | lia].
Qed.

End WithMemory.
