(** * Canonical ABI fragment for the echo slice

    The provider side of the boundary.  Where [echo_heap.v] says what a
    CertiRocq heap value means, this file says what a *canonical ABI* value
    means: the flat (pointer, length) representation of [list<u8>] and the
    return-area layout of [result<list<u8>, error>].

    Only the two type constructors the slice needs are covered.  The general
    development (sizes, alignments, [flatten], [load_value]/[store_value] for
    every [component_valtype]) is PLAN-authentikate.md §A; these definitions
    are the instances it must generalise, and the names here are chosen to be
    the ones that survive.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/CanonicalABI.md}} *)

From compcert Require Import lib.Integers.

From Wasm Require Import datatypes operations numerics bytes memory.

From Stdlib Require Import ZArith BinNat List Strings.String Strings.Byte Lia.

From WasmGlue Require Import wasm_mem certirocq_heap.
From wasmcomponents Require Import wit_ast echo_heap.

Import ListNotations.

Open Scope string_scope.

(** ** The interface being adapted

    [example:echo/echo@0.1.0], as a WIT world in this repository's AST.  The
    [error] variant is payload-free with a single case, which is what makes the
    error arm of the return area empty. *)
Definition echo_error_type : wit_type :=
  WitVariant [("unavailable"%string, None)].

Definition echo_func : wit_func := {|
  func_name := "echo"%string;
  func_params := [("data"%string, WitList (WitPrim WitU8))];
  func_results := [WitResult (Some (WitList (WitPrim WitU8)))
                             (Some (WitNamed "error"%string))]
|}.

Definition echo_interface : wit_interface := {|
  iface_name := "echo"%string;
  iface_types := [("error"%string, echo_error_type)];
  iface_funcs := [echo_func];
  iface_resources := []
|}.

Definition echo_world : wit_world := {|
  world_name := "echo-consumer"%string;
  world_imports := [("example:echo/echo@0.1.0"%string, echo_interface)];
  world_exports := []
|}.

(** ** Layout constants

    [result<list<u8>, error>] has a one-byte discriminant padded to the
    alignment of the largest payload.  [list<u8>] lowers to a pointer and a
    length, each 4 bytes aligned to 4, so the record is 12 bytes aligned to 4
    with the discriminant at +0, the pointer at +4 and the length at +8.

    These three numbers are hard-coded in every adapter's WAT (here
    [echo.wat], in authentikate [store.wat]'s header comment lists the same
    layouts).  Stating them once here is what lets a proof contradict the
    comment. *)
Definition result_bytes_size  : N := 12.
Definition result_bytes_align : N := 4.
Definition result_bytes_disc_off : N := 0.
Definition result_bytes_ptr_off  : N := 4.
Definition result_bytes_len_off  : N := 8.

(** Discriminants follow WIT declaration order: [ok] is 0, [err] is 1. *)
Definition disc_ok  : N := 0.
Definition disc_err : N := 1.

Section WithMemory.

Context `{BlockUpdateMemory}.

(** ** [list<u8>] as a lowered buffer

    [buffer_at m p bs] : the [List.length bs] bytes at [p] in [m] are [bs].  This is
    the canonical representation of [list<u8>] once lowered: contiguous,
    unaligned, with the length carried separately. *)
Definition buffer_at (m : meminst) (p : N) (bs : list Byte.byte) : Prop :=
  load m p 0%N (N.of_nat (List.length bs)) = Some (map compcert_byte_of_byte bs).

(** ** [result<list<u8>, error>] in a return area

    [canon_result_bytes m ret r] : the 12-byte return area at [ret] holds [r].

    [None] on the Rocq side corresponds to the [err] arm.  Because [error] is
    payload-free the arm carries no further data, so any non-zero discriminant
    witnesses it — which is also why an adapter may not read past the
    discriminant on that path. *)
Definition canon_result_bytes (m : meminst) (ret : N)
                              (r : option (list Byte.byte)) : Prop :=
  match r with
  | Some bs =>
      exists p,
        load_u8 m (ret + result_bytes_disc_off)%N = Some disc_ok /\
        load_i32 m (ret + result_bytes_ptr_off)%N
          = Some (VAL_int32 (N_to_i32 p)) /\
        load_i32 m (ret + result_bytes_len_off)%N
          = Some (VAL_int32 (N_to_i32 (N.of_nat (List.length bs)))) /\
        buffer_at m p bs
  | None =>
      exists d,
        load_u8 m (ret + result_bytes_disc_off)%N = Some d /\ d <> disc_ok
  end.

End WithMemory.

(** ** The provider specification

    What the host promises about the imported function, stated over the
    canonical ABI rather than over Rocq values.  This is the slice's instance
    of PLAN-authentikate.md §H(1): the one hypothesis that the end-to-end
    theorem is allowed to assume about the outside world.

    [echo_provider_spec impl] says [impl] is a function from the argument
    buffer to an optional answer; the adapter's job is to make the heap agree
    with it.  Note what it does *not* say: nothing about how long the answer
    is, and nothing about memory outside the return area and the buffer the
    answer points at. *)
Record echo_provider := {
  echo_impl : list Byte.byte -> option (list Byte.byte)
}.
