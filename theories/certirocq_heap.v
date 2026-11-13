(** * CertiRocq's Wasm heap layout

    Every adapter in this repository is merged into a module CertiRocq's Wasm
    backend emitted, so it inherits that module's conventions: which global
    holds the allocation pointer, how a constructor is represented, how wide a
    cell is.  This file states those conventions once, in the form the proofs
    use them.

    It deliberately does not depend on CertiRocq.  The definitions mirror
    {{https://github.com/womeier/CertiRocq}} at rev 6d34b50b
    ([theories/CodegenWasm/LambdaANF_to_Wasm.v] and
    [.../LambdaANF_to_Wasm_utils.v:1196-1214]) closely enough that the eventual
    bridge lemmas are by [reflexivity], so the development can proceed before
    CertiRocq becomes a dependency.

    The same conventions are what
    {{https://github.com/womeier/componentize-formal}} writes into every
    adapter it generates, as [bindgen/src/repr.rs]; where a constant appears
    in both, the comment names the one in [repr.rs]. *)

From Wasm Require Import datatypes operations numerics memory.

From Stdlib Require Import ZArith BinNat NArith.Nnat List Lia.

From wasmcomponents Require Import wasm_mem.

Import ListNotations.
Import Wasm_int.

(** ** Global indices

    The backend fixes these ([LambdaANF_to_Wasm.v:47-50]), and an adapter
    merged into that module must agree.  [repr.rs] [exports] names the same
    four for [wasm-merge] to resolve against: ["mem_ptr"], ["cap"], ["result"],
    ["out_of_mem"]. *)
Definition glob_mem_ptr    : globalidx := 0%N.
Definition glob_cap        : globalidx := 1%N.
Definition glob_result     : globalidx := 2%N.
Definition glob_out_of_mem : globalidx := 3%N.

(** ** Numeric helpers

    Definitionally equal to CertiRocq's ([LambdaANF_to_Wasm.v:80-95]). *)
Definition N_to_i32 (n : N) : i32 := Wasm_int.Int32.repr (BinInt.Z.of_N n).

Definition N_to_value (n : N) : value := VAL_num (VAL_int32 (N_to_i32 n)).

Definition Z_to_i64 (z : Z) : i64 := Wasm_int.Int64.repr z.

Definition Z_to_value64 (z : Z) : value := VAL_num (VAL_int64 (Z_to_i64 z)).

(** The modulus, as the bound an address has to stay under.  CertiRocq's
    invariant carries it as [(-1 < Z.of_N gmp < Wasm_int.Int32.modulus)%Z];
    stated over [N] it is one comparison. *)
Definition i32mod : N := Z.to_N Wasm_int.Int32.modulus.

(** Everything below mentions [meminst], which WasmCert parameterises by the
    [Memory] typeclass, so it lives in a section carrying that instance — the
    same way CertiRocq's utils do (there the instance arrives bundled inside
    [Context `{ho : host}]). *)
Section WithMemory.

Context `{Memory}.

(** ** Reading and writing a cell

    A cell is 4 bytes; a boxed primitive integer's payload is 8.  Both are
    [LambdaANF_to_Wasm_utils.v:1196-1214] verbatim. *)
Definition load_i32 (m : meminst) (addr : N) : option value_num :=
  match load m addr 0%N 4 with
  | None => None
  | Some bs => Some (wasm_deserialise bs T_i32)
  end.

Definition store_i32 (m : meminst) (addr : N) (v : value_num) : option meminst :=
  store m addr 0%N (serialise_num v) 4.

Definition load_i64 (m : meminst) (addr : N) : option value_num :=
  match load m addr 0%N 8 with
  | None => None
  | Some bs => Some (wasm_deserialise bs T_i64)
  end.

Definition store_i64 (m : meminst) (addr : N) (v : value_num) : option meminst :=
  store m addr 0%N (serialise_num v) 8.

(** A canonical-ABI discriminant is a single byte, which is what
    [i32.load8_u] reads. *)
Definition load_u8 (m : meminst) (a : N) : option N :=
  match load m a 0%N 1 with
  | Some [b] => Some (Z.to_N (Integers.Byte.unsigned b))
  | _ => None
  end.

(** ** Immediates

    A nullary constructor with ordinal [ord] is represented by the odd
    immediate [2 * ord + 1]; the low bit distinguishes immediates from
    pointers, which is why every cell is allocated at an even address.
    ([repr.rs]: the unboxed case, and the reason boxed and unboxed
    constructors are numbered independently.) *)
Definition unboxed (ord : N) : N := 2 * ord + 1.

(** [immediate v] holds when [v] is not a pointer. *)
Definition immediate (v : N) : Prop := exists ord, v = unboxed ord.

(** ** Framing

    The two facts that make adapter verification compositional: the bump
    allocator only ever moves [gmp] upwards and only ever writes above the old
    [gmp], so values already in the heap keep their meaning across a call.

    Stated at the byte level rather than per width, because an adapter reads
    both cells and 8-byte payloads below the bound and both have to be
    preserved. *)
Definition mem_agree_below (m m' : meminst) (bound : N) : Prop :=
  forall a len, (a + len <= bound)%N -> load m' a 0%N len = load m a 0%N len.

Lemma mem_agree_below_refl : forall m bound, mem_agree_below m m bound.
Proof. intros m bound a len Ha. reflexivity. Qed.

Lemma mem_agree_below_trans : forall m m' m'' bound,
  mem_agree_below m m' bound ->
  mem_agree_below m' m'' bound ->
  mem_agree_below m m'' bound.
Proof.
  intros m m' m'' bound H1 H2 a len Ha. rewrite (H2 a len Ha). apply H1. exact Ha.
Qed.

(** Agreeing below a bound shrinks to any smaller bound. *)
Lemma mem_agree_below_weaken : forall m m' bound bound',
  mem_agree_below m m' bound -> (bound' <= bound)%N -> mem_agree_below m m' bound'.
Proof. intros m m' b b' Hag Hle a len Ha. apply Hag. lia. Qed.

Lemma mem_agree_below_i32 : forall m m' bound a,
  mem_agree_below m m' bound -> (a + 4 <= bound)%N -> load_i32 m' a = load_i32 m a.
Proof.
  intros m m' bound a Hag Ha. unfold load_i32. rewrite (Hag a 4%N); [reflexivity | lia].
Qed.

Lemma mem_agree_below_i64 : forall m m' bound a,
  mem_agree_below m m' bound -> (a + 8 <= bound)%N -> load_i64 m' a = load_i64 m a.
Proof.
  intros m m' bound a Hag Ha. unfold load_i64. rewrite (Hag a 8%N); [reflexivity | lia].
Qed.

Lemma mem_agree_below_u8 : forall m m' bound a,
  mem_agree_below m m' bound -> (a + 1 <= bound)%N -> load_u8 m' a = load_u8 m a.
Proof.
  intros m m' bound a Hag Ha. unfold load_u8. rewrite (Hag a 1%N); [reflexivity | lia].
Qed.

(** ** Writing a cell

    [wasm_mem.v] has the generic facts about [store]; these are their
    instances at the two widths this representation uses, phrased against
    [load_i32]/[load_i64] and [mem_agree_below] so that the adapter proofs
    never have to mention [read_bytes].

    The [_frame] lemmas are the ones that matter: a store at or above a bound
    is invisible below it, which is how a representation predicate survives an
    allocation, and hence how an adapter stays composable with the generated
    code. *)

Lemma store_frame_gen : forall (m m' : meminst) (a bound : N) (bs : bytes) (l : nat),
  store m a 0%N bs l = Some m' ->
  (bound <= a)%N ->
  mem_agree_below m m' bound.
Proof.
  intros m m' a bound bs l Hs Hle x len Hx.
  replace len with (N.of_nat (N.to_nat len)) by apply N2Nat.id.
  eapply load_store_disjoint; [exact Hs |]. rewrite N2Nat.id. lia.
Qed.

Lemma store_i32_exists : forall (m : meminst) (a : N) (v : value_num),
  (a + 4 <= operations.mem_length m)%N ->
  exists m', store_i32 m a v = Some m'.
Proof.
  intros m a v Ha. unfold store_i32. apply store_exists. lia.
Qed.

Lemma store_i32_length : forall (m m' : meminst) (a : N) (v : value_num),
  store_i32 m a v = Some m' ->
  operations.mem_length m' = operations.mem_length m.
Proof.
  intros m m' a v Hs. unfold store_i32 in Hs. eapply store_length; exact Hs.
Qed.

Lemma load_i32_store_i32_same : forall (m m' : meminst) (a : N) (c : i32),
  store_i32 m a (VAL_int32 c) = Some m' ->
  load_i32 m' a = Some (VAL_int32 c).
Proof.
  intros m m' a c Hs. unfold load_i32.
  assert (Hld : load m' a 0%N (N.of_nat 4) = Some (serialise_i32 c)).
  { eapply load_store_same; [exact Hs | apply serialise_i32_length]. }
  change (N.of_nat 4) with 4%N in Hld.
  rewrite Hld. f_equal. apply deserialise_serialise_i32.
Qed.

Lemma load_i32_store_i32_disjoint : forall (m m' : meminst) (a b : N) (v : value_num),
  store_i32 m a v = Some m' ->
  ((b + 4 <= a)%N \/ (a + 4 <= b)%N) ->
  load_i32 m' b = load_i32 m b.
Proof.
  intros m m' a b v Hs Hdis. unfold load_i32, store_i32 in *.
  assert (Hld : load m' b 0%N (N.of_nat 4) = load m b 0%N (N.of_nat 4)).
  { eapply load_store_disjoint; [exact Hs | lia]. }
  change (N.of_nat 4) with 4%N in Hld. rewrite Hld. reflexivity.
Qed.

Lemma store_i32_frame : forall (m m' : meminst) (a bound : N) (v : value_num),
  store_i32 m a v = Some m' ->
  (bound <= a)%N ->
  mem_agree_below m m' bound.
Proof. intros m m' a bound v Hs Hle. eapply store_frame_gen; eauto. Qed.

Lemma store_i64_exists : forall (m : meminst) (a : N) (v : value_num),
  (a + 8 <= operations.mem_length m)%N ->
  exists m', store_i64 m a v = Some m'.
Proof.
  intros m a v Ha. unfold store_i64. apply store_exists. lia.
Qed.

Lemma store_i64_length : forall (m m' : meminst) (a : N) (v : value_num),
  store_i64 m a v = Some m' ->
  operations.mem_length m' = operations.mem_length m.
Proof.
  intros m m' a v Hs. unfold store_i64 in Hs. eapply store_length; exact Hs.
Qed.

Lemma load_i64_store_i64_same : forall (m m' : meminst) (a : N) (c : i64),
  store_i64 m a (VAL_int64 c) = Some m' ->
  load_i64 m' a = Some (VAL_int64 c).
Proof.
  intros m m' a c Hs. unfold load_i64.
  assert (Hld : load m' a 0%N (N.of_nat 8) = Some (serialise_i64 c)).
  { eapply load_store_same; [exact Hs | apply serialise_i64_length]. }
  change (N.of_nat 8) with 8%N in Hld.
  rewrite Hld. f_equal. apply deserialise_serialise_i64.
Qed.

Lemma load_i64_store_i64_disjoint : forall (m m' : meminst) (a b : N) (v : value_num),
  store_i64 m a v = Some m' ->
  ((b + 8 <= a)%N \/ (a + 8 <= b)%N) ->
  load_i64 m' b = load_i64 m b.
Proof.
  intros m m' a b v Hs Hdis. unfold load_i64, store_i64 in *.
  assert (Hld : load m' b 0%N (N.of_nat 8) = load m b 0%N (N.of_nat 8)).
  { eapply load_store_disjoint; [exact Hs | lia]. }
  change (N.of_nat 8) with 8%N in Hld. rewrite Hld. reflexivity.
Qed.

Lemma store_i64_frame : forall (m m' : meminst) (a bound : N) (v : value_num),
  store_i64 m a v = Some m' ->
  (bound <= a)%N ->
  mem_agree_below m m' bound.
Proof. intros m m' a bound v Hs Hle. eapply store_frame_gen; eauto. Qed.

(** ** Boxed primitive integers

    A [Uint63.int] is not an immediate: the backend boxes it as a pointer to
    an 8-byte payload holding the value as an [i64]
    ([LambdaANF_to_Wasm_correct.v:810], constructor [Rprim_v]; [repr.rs]
    [PRIM_PAYLOAD_SIZE]).  That is why the generated adapter stores its
    argument into the heap and passes the address, rather than passing the
    [i64] it received.

    [repr_prim m gmp addr z] : the cell at [addr] in [m], whose allocation
    pointer is [gmp], is the boxed primitive integer with unsigned value [z].
    The bound and the evenness are what the backend's invariant supplies and
    what an adapter must not break. *)
Definition repr_prim (m : meminst) (gmp addr : N) (z : Z) : Prop :=
  (addr + 8 <= gmp)%N /\
  (exists k, addr = 2 * k)%N /\
  load_i64 m addr = Some (VAL_int64 (Z_to_i64 z)).

Lemma repr_prim_gmp_mono : forall m gmp gmp' addr z,
  (gmp <= gmp')%N -> repr_prim m gmp addr z -> repr_prim m gmp' addr z.
Proof. intros m gmp gmp' addr z Hle (Hb & He & Hld). repeat split; auto. lia. Qed.

Lemma repr_prim_frame : forall m m' gmp addr z,
  mem_agree_below m m' gmp ->
  repr_prim m gmp addr z ->
  repr_prim m' gmp addr z.
Proof.
  intros m m' gmp addr z Hag (Hb & He & Hld). repeat split; auto.
  rewrite (mem_agree_below_i64 _ _ _ _ Hag); [exact Hld | lia].
Qed.

(** Writing the payload is how one is built: a store of an [i64] at an even
    address below the new allocation pointer. *)
Lemma repr_prim_store : forall m m' gmp addr z,
  store_i64 m addr (VAL_int64 (Z_to_i64 z)) = Some m' ->
  (addr + 8 <= gmp)%N ->
  (exists k, addr = 2 * k)%N ->
  repr_prim m' gmp addr z.
Proof.
  intros m m' gmp addr z Hs Hb He. repeat split; auto.
  eapply load_i64_store_i64_same; exact Hs.
Qed.

End WithMemory.
