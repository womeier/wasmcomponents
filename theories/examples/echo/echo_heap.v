(** * CertiRocq heap values, for the echo slice

    [certirocq_heap.v] says how the backend lays out the heap in general.
    This file specialises it to the two type constructors the echo slice
    needs: [list byte] (CertiRocq's [bytestring.String]) and
    [option (list byte)].

    The relations below mirror [repr_val_LambdaANF_Wasm] from
    {{https://github.com/womeier/CertiRocq}} at rev 6d34b50b,
    [theories/CodegenWasm/LambdaANF_to_Wasm_correct.v:813], specialised to
    those two, so the development can proceed before CertiRocq becomes a
    dependency of this repository.

    ** ABI contract owed

    Specialising [repr_val_LambdaANF_Wasm] means baking in facts about the
    [cenv] of the compiled program.  When CertiRocq is added as a dependency
    these become side conditions to discharge (by reflection on the generated
    [cenv]) rather than assumptions:

    - [EmptyString] is nullary, so it is unboxed with ordinal 0, i.e. the
      immediate [1].
    - [String] has arity 2, so it is boxed with ordinal 0: a cell holding the
      tag at +0, the head byte at +4 and the tail at +8.
    - the 256 constructors of [byte] are nullary with ordinals equal to their
      numeric value, so byte [b] is the immediate [2 * b + 1].
    - [None] is nullary (unboxed ordinal 0, immediate [1]) and [Some] has
      arity 1 (boxed ordinal 0): a cell with the tag at +0 and the payload
      at +4.

    Boxed and unboxed constructors are numbered separately, which is why both
    [None] and [Some] carry ordinal 0 without conflict.  The WAT in
    [echo.wat] — and every adapter in authentikate — assumes exactly this. *)

From Wasm Require Import datatypes operations numerics memory.

From Stdlib Require Import ZArith BinNat List Strings.Byte Lia.

From WasmGlue Require Import wasm_mem certirocq_heap.

Import ListNotations.
Import Wasm_int.

(** The adapter's own global, which [$lower] uses to return a length
    alongside a pointer.  The backend's four stop at index 3
    ([certirocq_heap.v]). *)
Definition glob_buf_len : globalidx := 4%N.

Section WithMemory.

Context `{BlockUpdateMemory}.

(** ** Representation of [list byte]

    [repr_bytes m gmp v bs] : the i32 value [v], read in memory [m] whose
    allocation pointer is [gmp], represents the bytestring [bs].

    The bounds ([addr + 12 <= gmp]) and the evenness of [addr] are what the
    backend's invariant supplies and what an adapter must not break. *)
Inductive repr_bytes (m : meminst) (gmp : N) : N -> list byte -> Prop :=
| repr_bytes_nil :
    repr_bytes m gmp (unboxed 0) []
| repr_bytes_cons : forall addr b bs tail,
    (addr + 12 <= gmp)%N ->
    (exists k, addr = 2 * k)%N ->
    load_i32 m addr = Some (VAL_int32 (N_to_i32 0)) ->
    load_i32 m (addr + 4)%N
      = Some (VAL_int32 (N_to_i32 (unboxed (Byte.to_N b)))) ->
    load_i32 m (addr + 8)%N = Some (VAL_int32 (N_to_i32 tail)) ->
    repr_bytes m gmp tail bs ->
    repr_bytes m gmp addr (b :: bs).

(** ** Representation of [option (list byte)]

    The result type of [ffi_echo].  A [Some] cell is 8 bytes, which is what
    [$some] in [common.wat] allocates. *)
Inductive repr_option_bytes (m : meminst) (gmp : N) : N -> option (list byte) -> Prop :=
| repr_none :
    repr_option_bytes m gmp (unboxed 0) None
| repr_some : forall addr payload bs,
    (addr + 8 <= gmp)%N ->
    (exists k, addr = 2 * k)%N ->
    load_i32 m addr = Some (VAL_int32 (N_to_i32 0)) ->
    load_i32 m (addr + 4)%N = Some (VAL_int32 (N_to_i32 payload)) ->
    repr_bytes m gmp payload bs ->
    repr_option_bytes m gmp addr (Some bs).

(** ** Frame properties

    The reason the primitive import contract in PLAN-authentikate.md §E(3) is
    statable: the bump allocator only ever moves [gmp] upwards and only ever
    writes above the old [gmp], so values already in the heap keep their
    meaning across an adapter call. *)

Lemma repr_bytes_gmp_mono : forall m gmp gmp' v bs,
  (gmp <= gmp')%N ->
  repr_bytes m gmp v bs ->
  repr_bytes m gmp' v bs.
Proof.
  intros m gmp gmp' v bs Hle Hrepr.
  induction Hrepr as [| addr b bs tail Hbound Heven Htag Hhead Htail IH].
  - constructor.
  - econstructor; eauto. lia.
Qed.

Lemma repr_option_bytes_gmp_mono : forall m gmp gmp' v o,
  (gmp <= gmp')%N ->
  repr_option_bytes m gmp v o ->
  repr_option_bytes m gmp' v o.
Proof.
  intros m gmp gmp' v o Hle Hrepr.
  destruct Hrepr as [| addr payload bs Hbound Heven Htag Hpayload Hbs].
  - constructor.
  - econstructor; eauto; try lia.
    eapply repr_bytes_gmp_mono; eauto.
Qed.

Lemma repr_bytes_frame : forall m m' gmp v bs,
  mem_agree_below m m' gmp ->
  repr_bytes m gmp v bs ->
  repr_bytes m' gmp v bs.
Proof.
  intros m m' gmp v bs Hagree Hrepr.
  induction Hrepr as [| addr b bs tail Hbound Heven Htag Hhead Htail IH].
  - constructor.
  - econstructor; eauto.
    + rewrite (mem_agree_below_i32 _ _ _ _ Hagree); [ exact Htag | lia ].
    + rewrite (mem_agree_below_i32 _ _ _ _ Hagree); [ exact Hhead | lia ].
    + rewrite (mem_agree_below_i32 _ _ _ _ Hagree); [ exact Htail | lia ].
Qed.

Lemma repr_option_bytes_frame : forall m m' gmp v o,
  mem_agree_below m m' gmp ->
  repr_option_bytes m gmp v o ->
  repr_option_bytes m' gmp v o.
Proof.
  intros m m' gmp v o Hagree Hrepr.
  destruct Hrepr as [| addr payload bs Hbound Heven Htag Hpayload Hbs].
  - constructor.
  - econstructor; eauto.
    + rewrite (mem_agree_below_i32 _ _ _ _ Hagree); [ exact Htag | lia ].
    + rewrite (mem_agree_below_i32 _ _ _ _ Hagree); [ exact Hpayload | lia ].
    + eapply repr_bytes_frame; eauto.
Qed.

End WithMemory.
