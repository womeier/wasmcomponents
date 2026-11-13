(** * The [id63] adapter as a WasmCert module

    A faithful transcription of [adapter.wat] in this directory, which is not
    hand-written: [componentize-formal generate] emitted it from
    [wit/world.wit].  That file is the source of truth — it is what
    [wasm-tools parse] assembles into the adapter core module that the
    component embeds next to the one CertiRocq emits ([core_instantiate.v]);
    this file is what the proofs in [id63_correct.v] talk about.

    The two are kept in agreement by hand for now.  Closing that gap is the
    same job as PLAN-authentikate.md §F(5): assemble the WAT with
    [wasm-tools parse], parse the resulting [.wasm] with WasmCert's verified
    binary parser, and check the AST it yields equals [id63_module] below.
    Until then the transcription is trusted, and it is the only part of this
    slice that is.

    ** What is being verified, and why this example

    [componentize-formal]'s own TODO lists verified glue as unsupported: the
    adapter it generates is trusted.  This slice is that gap closed for the
    smallest world the generator accepts — one export, one argument, a type
    with no recursion in it.

    ** Index spaces

    The adapter stays its own module — the component instantiates it with the
    backend's exports rather than merging the two — so its index spaces are
    its own, and imports sit at the bottom of each of them
    ([core_instantiate.v], [alloc_module_shape]).  With imports in the order
    [memory], [mem_ptr], [result], [out_of_mem], [main_function], [table]:
    globals 0-2 are the backend's [mem_ptr], [result], [out_of_mem]; function
    0 is [main_function]; the adapter's own three bodies follow at 1-3.  The
    [layout] record below still carries the function and type indices as
    parameters — the bodies stay readable, the proofs stay index-generic —
    but unlike under [wasm-merge] there is now a theorem, not a convention of
    binaryen's, that pins what instantiates them.

    Locals are numbered after parameters, per Wasm. *)

From Wasm Require Import datatypes operations numerics.

From Stdlib Require Import ZArith BinNat List Strings.String.

From wasmcomponents Require Import certirocq_heap id63_abi.

Import ListNotations.

(** ** Shorthands

    These keep the bodies below diffable against [adapter.wat] by eye, which
    is the only check there is until the parser closes the loop. *)
Definition i32c (n : N) : basic_instruction :=
  BI_const_num (VAL_int32 (N_to_i32 n)).

Definition i64c (z : Z) : basic_instruction :=
  BI_const_num (VAL_int64 (Z_to_i64 z)).

(** [i32.const -8], the mask that rounds an address down to a multiple of 8. *)
Definition i32c_neg8 : basic_instruction :=
  BI_const_num (VAL_int32 (Wasm_int.Int32.repr (-8)%Z)).

Definition add   : basic_instruction := BI_binop T_i32 (Binop_i BOI_add).
Definition and_  : basic_instruction := BI_binop T_i32 (Binop_i BOI_and).
Definition mul   : basic_instruction := BI_binop T_i32 (Binop_i BOI_mul).
Definition gt_u  : basic_instruction := BI_relop T_i32 (Relop_i (ROI_gt SX_U)).
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
    first and the adapter defines none of its own, so it is index 0. *)
Definition tbl_funcs : tableidx := 0%N.

(** Page size, as the adapter spells it. *)
Definition page_bytes : N := 65536.

(** The adapter module's numbering, as described above.  Bundled rather than
    left as section variables so that every body below says which index it
    means. *)
Record layout := {
  ly_ty_ok      : typeidx;
  ly_ty_err     : typeidx;
  ly_ty_export  : typeidx;
  ly_ty_closure : typeidx;
  ly_ok         : funcidx;
  ly_err        : funcidx;
  ly_main       : funcidx
}.

Section Adapter.

Variable L : layout.

(** ** [$ok] : (ret, val) -> ()

    params: ret = 0, val = 1.

    Writes the [ok] arm of the return area: discriminant 0 at +0, the payload
    at +8 ([id63_abi.v]). *)
Definition ok_body : expr :=
  [ BI_local_get 0%N ; i32c 0 ; store_byte 0
  ; BI_local_get 0%N ; BI_local_get 1%N ; store_i64_at 8 ].

Definition ok_func : module_func := {|
  modfunc_type := ly_ty_ok L;
  modfunc_locals := [];
  modfunc_body := ok_body
|}.

(** ** [$err] : (ret, case) -> ()

    params: ret = 0, case = 1.

    The error half is an enum, so its payload is a bare discriminant and goes
    where the [u64] would have. *)
Definition err_body : expr :=
  [ BI_local_get 0%N ; i32c 1 ; store_byte 0
  ; BI_local_get 0%N ; BI_local_get 1%N ; store_byte 8 ].

Definition err_func : module_func := {|
  modfunc_type := ly_ty_err L;
  modfunc_locals := [];
  modfunc_body := err_body
|}.

(** ** The export : (val : i64) -> i32

    params: val = 0.  locals: ret = 1, arg = 2, closure = 3.

    Split into the pieces the proof branches on. *)

(** Round [glob_mem_ptr] up to a multiple of 8, take 24 bytes — 16 for the
    return area, 8 for the boxed argument — and leave [glob_mem_ptr] past
    them.  Runs before [$main_function], so the heap is still empty. *)
Definition prologue : expr :=
  [ BI_global_get glob_mem_ptr ; i32c 7 ; add ; i32c_neg8 ; and_
  ; BI_global_set glob_mem_ptr
  ; BI_global_get glob_mem_ptr ; BI_local_set 1%N
  ; BI_local_get 1%N ; i32c 16 ; add ; BI_local_set 2%N
  ; BI_local_get 1%N ; i32c 24 ; add ; BI_global_set glob_mem_ptr ].

(** Nothing bounds-checks what the adapter takes for itself, and a store past
    the end of the memory would trap instead of answering.  Reaching the
    [unreachable] means there is no room for the return area at the bottom of
    a fresh heap, which cannot be reported through a return area that does not
    fit — the one failure with nowhere to go. *)
Definition room_check : expr :=
  [ BI_global_get glob_mem_ptr ; BI_memory_size ; i32c page_bytes ; mul ; gt_u
  ; BI_if (BT_valtype None) [ BI_unreachable ] [] ].

(** Answering [err] and returning, as both failure arms do. *)
Definition fail_with (case : N) : expr :=
  [ BI_local_get 1%N ; i32c case ; BI_call (ly_err L)
  ; BI_local_get 1%N ; BI_return ].

(** A [u64] of 2^63 or more has no [Uint63.int], so it is rejected before the
    program runs rather than silently losing its top bit.  Read as an [i64]
    that is exactly the negative half. *)
Definition range_check : expr :=
  [ BI_local_get 0%N ; i64c 0 ; lt_s64
  ; BI_if (BT_valtype None) (fail_with (error_code OutOfRange)) [] ].

(** [glob_out_of_mem] means the value in [glob_result] was never written, so
    neither of these checks is optional. *)
Definition oom_check : expr :=
  [ BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) (fail_with (error_code OutOfMemory)) [] ].

(** [$main_function] runs the whole program and leaves the export closure in
    [glob_result].  It allocates, so it can be what exhausts the memory. *)
Definition run_main : expr :=
  ([ BI_call (ly_main L) ] ++ oom_check
     ++ [ BI_global_get glob_result ; BI_local_set 3%N ])%list.

(** A primitive int is boxed, so what gets passed is a pointer to the 8-byte
    payload, not the value ([certirocq_heap.v], [repr_prim]).  Closure layout:
    [tag, func_idx, env_ptr] in 4-byte cells. *)
Definition call_closure : expr :=
  [ BI_local_get 2%N ; BI_local_get 0%N ; store_i64_at 0
  ; BI_local_get 3%N ; load_word 8
  ; BI_local_get 2%N
  ; BI_local_get 3%N ; load_word 4
  ; BI_call_indirect tbl_funcs (ly_ty_closure L) ].

(** [glob_result] holds a pointer to the 8-byte payload, not the value. *)
Definition epilogue : expr :=
  [ BI_local_get 1%N ; BI_global_get glob_result ; load_i64_at 0 ; BI_call (ly_ok L)
  ; BI_local_get 1%N ].

Definition export_body : expr :=
  (prologue ++ room_check ++ range_check ++ run_main
            ++ call_closure ++ oom_check ++ epilogue)%list.

Definition export_func : module_func := {|
  modfunc_type := ly_ty_export L;
  modfunc_locals := repeat (T_num T_i32) 3;
  modfunc_body := export_body
|}.

End Adapter.
