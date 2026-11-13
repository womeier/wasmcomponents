(** * Canonical ABI for [result<u64, error>]

    The component side of the [id63] boundary.  [certirocq_heap.v] says what a
    CertiRocq heap value is; this file says what the *return area* is — the
    bytes the host reads back once the export returns.

    Everything here is forced.  The layout is what [wasm-tools component new]
    computes for [result<u64, error>], and the discriminants are WIT
    declaration order; the generated [adapter.wat] writes exactly these
    offsets, with [bindgen/src/repr.rs] as the single place the two agree.
    Stating them here is what lets a proof contradict the adapter rather than
    restate it.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/CanonicalABI.md}} *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics bytes memory.

From Stdlib Require Import ZArith BinNat List Strings.String Lia.

From WasmGlue Require Import wasm_mem certirocq_heap.
From wasmcomponents Require Import wit_ast.

Import ListNotations.

Open Scope string_scope.

(** ** The interface being adapted

    [wit/world.wit] in this directory, as far as this repository's AST can
    carry it:

    <<
    package local:demo;

    world wrapper-world {
        use certirocq:abi/errors@0.1.0.{error};

        export id63: func(val: u64) -> result<u64, error>;
    }
    >>

    [wit_world] can only hold interfaces ([wit_ast.v:96]), so a world whose
    export is a bare function has no encoding yet; [id63_func] below is the
    signature itself, and the world is left for whenever [wit_world] grows a
    [world_export_funcs] field. *)

(** The shipped error enum, [bindgen/wit/certirocq-abi.wit].  Case order *is*
    the discriminant, which is why the package carrying it is versioned. *)
Definition certirocq_error_type : wit_type :=
  WitEnum ["out-of-memory"; "out-of-range"].

Definition id63_func : wit_func := {|
  func_name := "id63";
  func_params := [("val", WitPrim WitU64)];
  func_results := [WitResult (Some (WitPrim WitU64))
                             (Some (WitNamed "error"))]
|}.

Definition certirocq_abi_interface : wit_interface := {|
  iface_name := "errors";
  iface_types := [("error", certirocq_error_type)];
  iface_funcs := [];
  iface_resources := []
|}.

(** ** Layout constants

    [result<u64, error>] is a variant whose largest payload is a [u64], so it
    is 8-aligned, the discriminant byte at +0 is padded out to the payload's
    alignment, and the whole thing is 16 bytes.  The error arm's payload is an
    enum, which lowers to its discriminant alone — one byte, at the same +8.

    The alignment is why the adapter's prologue rounds [glob_mem_ptr] up to a
    multiple of 8 before taking the return area: a misaligned return area is
    rejected by the host, and the backend only keeps [glob_mem_ptr] 4-aligned. *)
Definition result_u64_size      : N := 16.
Definition result_u64_align     : N := 8.
Definition result_u64_disc_off  : N := 0.
Definition result_u64_payload_off : N := 8.

(** [result] declares [ok] before [err]. *)
Definition disc_ok  : N := 0.
Definition disc_err : N := 1.

(** ** The failure channel

    Neither case belongs to the Rocq function: [interface.v] asks for
    [int -> int] and a Rocq function can neither run out of memory in its own
    type nor know that a value has to fit a WIT width.  Both are the
    adapter's, which is why every export carries them. *)
Inductive abi_error :=
  | OutOfMemory   (** the bump allocator could not grow the memory *)
  | OutOfRange    (** a [u64] of 2^63 or more has no [Uint63.int] *).

Definition error_code (e : abi_error) : N :=
  match e with
  | OutOfMemory => 0
  | OutOfRange  => 1
  end.

Section WithMemory.

Context `{Memory}.

(** [canon_result_u64 m ret r] : the 16-byte return area at [ret] in [m]
    holds [r].

    Note what the [err] arm does *not* say: nothing about the payload bytes
    beyond the enum discriminant, and nothing about the eight bytes at +8 on
    the [ok] arm beyond their value — the adapter leaves the padding at +1..+7
    untouched, and the host does not read it. *)
Definition canon_result_u64 (m : meminst) (ret : N)
                            (r : i64 + abi_error) : Prop :=
  match r with
  | inl v =>
      load_u8 m (ret + result_u64_disc_off)%N = Some disc_ok /\
      load_i64 m (ret + result_u64_payload_off)%N = Some (VAL_int64 v)
  | inr e =>
      load_u8 m (ret + result_u64_disc_off)%N = Some disc_err /\
      load_u8 m (ret + result_u64_payload_off)%N = Some (error_code e)
  end.

End WithMemory.
