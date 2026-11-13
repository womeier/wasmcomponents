(** * The echo adapter as a WasmCert module

    A faithful transcription of [echo.wat] into WasmCert's AST.  [echo.wat] is
    the source of truth (it is what a build pipeline assembles and what
    [wasm-tools] validates); this file is what the proofs in [echo_correct.v]
    talk about.

    The two are kept in agreement by hand for now.  PLAN-authentikate.md §F(5)
    is how that gap closes: assemble the WAT with [wasm-tools parse], parse the
    resulting [.wasm] with WasmCert's verified binary parser, and check the
    AST it yields equals [echo_adapter] below.  Until then the transcription is
    trusted, and it is the only part of the slice that is.

    Indices matter and are fixed here:
    - globals 0-3 are CertiRocq's ([echo_heap.v]), imported; global 4 is
      [$buf_len], this module's own.
    - the imported provider function is funcidx 0, so the defined functions
      start at 1.
    - locals are numbered after parameters, per Wasm. *)

From Wasm Require Import datatypes operations numerics.

From Stdlib Require Import ZArith BinNat List Strings.String.

From WasmGlue Require Import certirocq_heap.
From wasmcomponents Require Import echo_heap echo_abi.

Import ListNotations.

(** ** Shorthands

    These keep the bodies below readable enough to diff against [echo.wat] by
    eye, which is the only check there is until the parser closes the loop. *)
Definition i32c (n : N) : basic_instruction :=
  BI_const_num (VAL_int32 (N_to_i32 n)).

(** The all-ones constant, i.e. [i32.const -1]; used both for the alignment
    mask and for the [memory.grow] failure test. *)
Definition i32_minus_one : basic_instruction :=
  BI_const_num (VAL_int32 (Wasm_int.Int32.repr (-1)%Z)).

Definition add  : basic_instruction := BI_binop T_i32 (Binop_i BOI_add).
Definition sub  : basic_instruction := BI_binop T_i32 (Binop_i BOI_sub).
Definition and_ : basic_instruction := BI_binop T_i32 (Binop_i BOI_and).
Definition xor  : basic_instruction := BI_binop T_i32 (Binop_i BOI_xor).
Definition shl  : basic_instruction := BI_binop T_i32 (Binop_i BOI_shl).
Definition shr_u : basic_instruction := BI_binop T_i32 (Binop_i (BOI_shr SX_U)).
Definition eq_  : basic_instruction := BI_relop T_i32 (Relop_i ROI_eq).
Definition eqz  : basic_instruction := BI_testop T_i32 TO_eqz.
Definition gt_u : basic_instruction := BI_relop T_i32 (Relop_i (ROI_gt SX_U)).

(** Word load/store at a dynamic address, [memarg] as the backend emits it
    ([LambdaANF_to_Wasm.v:185]): offset 0, align 2 (= log2 4). *)
Definition load_word (off : N) : basic_instruction :=
  BI_load T_i32 None {| memarg_offset := off; memarg_align := 2%N |}.

Definition store_word (off : N) : basic_instruction :=
  BI_store T_i32 None {| memarg_offset := off; memarg_align := 2%N |}.

Definition load_byte_u : basic_instruction :=
  BI_load T_i32 (Some (Tp_i8, SX_U)) {| memarg_offset := 0%N; memarg_align := 0%N |}.

Definition store_byte : basic_instruction :=
  BI_store T_i32 (Some Tp_i8) {| memarg_offset := 0%N; memarg_align := 0%N |}.

(** ** Type and function indices *)
Definition ty_i32i32i32_void : typeidx := 0%N.  (** the provider import *)
Definition ty_i32i32_i32     : typeidx := 1%N.  (** $alloc, $lift_bytes *)
Definition ty_i32_i32        : typeidx := 2%N.  (** $list_len, $lower, $some, $ffi_echo *)
Definition ty_i32i32_void    : typeidx := 3%N.  (** the exported wrapper *)
Definition ty_realloc        : typeidx := 4%N.  (** $cabi_realloc *)

Definition fidx_echo       : funcidx := 0%N.    (** imported *)
Definition fidx_alloc      : funcidx := 1%N.
Definition fidx_list_len   : funcidx := 2%N.
Definition fidx_lower      : funcidx := 3%N.
Definition fidx_lift_bytes : funcidx := 4%N.
Definition fidx_some       : funcidx := 5%N.
Definition fidx_ffi_echo   : funcidx := 6%N.
Definition fidx_export     : funcidx := 7%N.
Definition fidx_cabi_realloc : funcidx := 8%N.

(** ** [$alloc] : (n, align) -> ptr

    params: n = 0, align = 1.  locals: p = 2, end = 3, have = 4, pages = 5.

    Bump allocation with page growth, from [common.wat].  On failure it sets
    [glob_out_of_mem] and returns 0; every caller checks the flag rather than
    the returned pointer. *)
(** The straight-line head: computes [p], [end] and [have] and leaves the
    overflow test [end >u have] on the stack.  Split out of [alloc_body] so
    that the proof can discharge it once and then branch three ways. *)
Definition alloc_head : expr :=
  (* p := (mem_ptr + (align - 1)) & ~(align - 1) *)
  [ BI_global_get glob_mem_ptr
  ; BI_local_get 1%N ; i32c 1 ; sub
  ; add
  ; BI_local_get 1%N ; i32c 1 ; sub ; i32_minus_one ; xor
  ; and_
  ; BI_local_set 2%N
  (* end := p + n *)
  ; BI_local_get 2%N ; BI_local_get 0%N ; add ; BI_local_set 3%N
  (* have := memory.size * 64Ki *)
  ; BI_memory_size ; i32c 16 ; shl ; BI_local_set 4%N
  (* if end > have then grow *)
  ; BI_local_get 3%N ; BI_local_get 4%N ; gt_u ].

(** The [then] arm: grow by as many pages as are missing, or signal OOM. *)
Definition alloc_grow : expr :=
  (* pages := ((end - have) >> 16) + 1 *)
  [ BI_local_get 3%N ; BI_local_get 4%N ; sub
  ; i32c 16 ; shr_u ; i32c 1 ; add ; BI_local_set 5%N
  ; BI_local_get 5%N ; BI_memory_grow ; i32_minus_one ; eq_
  ; BI_if (BT_valtype None)
      [ i32c 1 ; BI_global_set glob_out_of_mem
      ; i32c 0 ; BI_return ]
      [] ].

(** mem_ptr := end; return p *)
Definition alloc_tail : expr :=
  [ BI_local_get 3%N ; BI_global_set glob_mem_ptr
  ; BI_local_get 2%N ].

Definition alloc_body : expr :=
  (alloc_head ++ [ BI_if (BT_valtype None) alloc_grow [] ] ++ alloc_tail)%list.

Definition alloc_func : module_func := {|
  modfunc_type := ty_i32i32_i32;
  modfunc_locals := repeat (T_num T_i32) 4;
  modfunc_body := alloc_body
|}.

(** ** [$list_len] : cell -> length

    params: l = 0.  locals: n = 1.

    Walks the bytestring spine until it meets the [EmptyString] immediate,
    which is detected by its low bit.  The first of the slice's two loops. *)
Definition list_len_loop : expr :=
  [ BI_local_get 0%N ; i32c 1 ; and_ ; BI_br_if 1%N
  ; BI_local_get 1%N ; i32c 1 ; add ; BI_local_set 1%N
  ; BI_local_get 0%N ; load_word 8 ; BI_local_set 0%N
  ; BI_br 0%N ].

Definition list_len_body : expr :=
  [ BI_block (BT_valtype None) [ BI_loop (BT_valtype None) list_len_loop ]
  ; BI_local_get 1%N ].

Definition list_len_func : module_func := {|
  modfunc_type := ty_i32_i32;
  modfunc_locals := repeat (T_num T_i32) 1;
  modfunc_body := list_len_body
|}.

(** ** [$lower] : cell -> ptr, with the length left in [$buf_len]

    params: l = 0.  locals: len = 1, p = 2, i = 3.

    Flattens a heap bytestring into a fresh canonical buffer.  The length goes
    through a global because a Wasm function returning two values would not
    match the shape CertiRocq's registered primitives use. *)
Definition lower_body : expr :=
  [ BI_local_get 0%N ; BI_call fidx_list_len ; BI_local_set 1%N
  ; BI_local_get 1%N ; BI_global_set glob_buf_len
  ; BI_local_get 1%N ; i32c 1 ; BI_call fidx_alloc ; BI_local_set 2%N
  ; BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) [ i32c 0 ; BI_return ] []
  ; BI_block (BT_valtype None)
      [ BI_loop (BT_valtype None)
          [ BI_local_get 0%N ; i32c 1 ; and_ ; BI_br_if 1%N
          ; BI_local_get 2%N ; BI_local_get 3%N ; add
          ; BI_local_get 0%N ; load_word 4 ; i32c 1 ; shr_u
          ; store_byte
          ; BI_local_get 3%N ; i32c 1 ; add ; BI_local_set 3%N
          ; BI_local_get 0%N ; load_word 8 ; BI_local_set 0%N
          ; BI_br 0%N ] ]
  ; BI_local_get 2%N ].

Definition lower_func : module_func := {|
  modfunc_type := ty_i32_i32;
  modfunc_locals := repeat (T_num T_i32) 3;
  modfunc_body := lower_body
|}.

(** ** [$lift_bytes] : (ptr, len) -> cell

    params: p = 0, len = 1.  locals: acc = 2, cell = 3.

    Builds the heap bytestring back to front so that each cell's tail is
    already in hand; [acc] starts as the [EmptyString] immediate.  On OOM it
    returns that same immediate, which is why an out-of-memory [$lift_bytes]
    silently yields the empty string unless the caller checks the flag — and
    [$ffi_echo] does check it. *)
Definition lift_bytes_body : expr :=
  [ i32c 1 ; BI_local_set 2%N
  ; BI_block (BT_valtype None)
      [ BI_loop (BT_valtype None)
          [ BI_local_get 1%N ; eqz ; BI_br_if 1%N
          ; BI_local_get 1%N ; i32c 1 ; sub ; BI_local_set 1%N
          ; i32c 12 ; i32c 4 ; BI_call fidx_alloc ; BI_local_set 3%N
          ; BI_global_get glob_out_of_mem
          ; BI_if (BT_valtype None) [ i32c 1 ; BI_return ] []
          (* cell[0] := tag 0 *)
          ; BI_local_get 3%N ; i32c 0 ; store_word 0
          (* cell[4] := 2 * byte + 1 *)
          ; BI_local_get 3%N
          ; BI_local_get 0%N ; BI_local_get 1%N ; add ; load_byte_u
          ; i32c 1 ; shl ; i32c 1 ; BI_binop T_i32 (Binop_i BOI_or)
          ; store_word 4
          (* cell[8] := acc; acc := cell *)
          ; BI_local_get 3%N ; BI_local_get 2%N ; store_word 8
          ; BI_local_get 3%N ; BI_local_set 2%N
          ; BI_br 0%N ] ]
  ; BI_local_get 2%N ].

Definition lift_bytes_func : module_func := {|
  modfunc_type := ty_i32i32_i32;
  modfunc_locals := repeat (T_num T_i32) 2;
  modfunc_body := lift_bytes_body
|}.

(** ** [$some] : payload -> cell

    params: x = 0.  locals: cell = 1.

    An 8-byte boxed [Some].  Returns the [None] immediate on OOM, for the same
    reason [$lift_bytes] does. *)
(** Split at the call to [$alloc], the way [alloc_head]/[alloc_tail] are:
    [some_spec] applies [alloc_spec] to the head and steps through the tail,
    and neither proof has to spell the other half out. *)
Definition some_alloc : expr := [ i32c 8 ; i32c 4 ; BI_call fidx_alloc ].

Definition some_tail : expr :=
  [ BI_local_set 1%N
  ; BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) [ i32c 1 ; BI_return ] []
  ; BI_local_get 1%N ; i32c 0 ; store_word 0
  ; BI_local_get 1%N ; BI_local_get 0%N ; store_word 4
  ; BI_local_get 1%N ].

(** The instructions after the [if], which is what a [return] out of the
    error arm has to unwind past. *)
Definition some_stores : expr :=
  [ BI_local_get 1%N ; i32c 0 ; store_word 0
  ; BI_local_get 1%N ; BI_local_get 0%N ; store_word 4
  ; BI_local_get 1%N ].

Definition some_body : expr := (some_alloc ++ some_tail)%list.

Definition some_func : module_func := {|
  modfunc_type := ty_i32_i32;
  modfunc_locals := repeat (T_num T_i32) 1;
  modfunc_body := some_body
|}.

(** ** [$ffi_echo] : arg -> option bytes

    params: arg = 0.  locals: p = 1, len = 2, ret = 3.

    The adapter proper: allocate a return area, lower the argument, call the
    provider, and translate the canonical answer back into the heap.  Every
    failure path answers with the [None] immediate [1]. *)
Definition ffi_echo_body : expr :=
  [ i32c 12 ; i32c 4 ; BI_call fidx_alloc ; BI_local_set 3%N
  ; BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) [ i32c 1 ; BI_return ] []
  ; BI_local_get 0%N ; BI_call fidx_lower ; BI_local_set 1%N
  ; BI_global_get glob_buf_len ; BI_local_set 2%N
  ; BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) [ i32c 1 ; BI_return ] []
  ; BI_local_get 1%N ; BI_local_get 2%N ; BI_local_get 3%N
  ; BI_call fidx_echo
  ; BI_local_get 3%N ; load_byte_u
  ; BI_if (BT_valtype (Some (T_num T_i32)))
      [ i32c 1 ]
      [ BI_local_get 3%N ; load_word 4
      ; BI_local_get 3%N ; load_word 8
      ; BI_call fidx_lift_bytes
      ; BI_call fidx_some ] ].

Definition ffi_echo_func : module_func := {|
  modfunc_type := ty_i32_i32;
  modfunc_locals := repeat (T_num T_i32) 3;
  modfunc_body := ffi_echo_body
|}.

(** ** The exported registered primitive

    params: force = 0, arg = 1.  No locals.

    CertiRocq calls this, ignores its (absent) return value and reads
    [glob_result].  The leading [force] argument is rev 6d34b50b's
    lazy-force dummy and is ignored. *)
Definition export_body : expr :=
  [ BI_local_get 1%N ; BI_call fidx_ffi_echo ; BI_global_set glob_result ].

Definition export_func : module_func := {|
  modfunc_type := ty_i32i32_void;
  modfunc_locals := [];
  modfunc_body := export_body
|}.

(** ** [$cabi_realloc] : (old_ptr, old_size, align, new_size) -> ptr

    params: old_ptr = 0, old_size = 1, align = 2, new_size = 3.
    locals: p = 4, i = 5, n = 6.

    The allocator the *host* calls, not this module.  Lowering an import whose
    result contains a [list] needs a [realloc] in the caller's instance so the
    answer can be written into the caller's memory; without this export the
    component's [canon lower] for [echo] cannot be formed at all.

    Copies [min(old_size, new_size)] bytes and never frees, so a shrink is a
    fresh allocation and a copy.  Returns 0 on OOM with the flag set. *)
Definition cabi_realloc_body : expr :=
  [ BI_local_get 3%N ; BI_local_get 2%N ; BI_call fidx_alloc ; BI_local_set 4%N
  ; BI_global_get glob_out_of_mem
  ; BI_if (BT_valtype None) [ i32c 0 ; BI_return ] []
  ; BI_local_get 1%N ; BI_local_set 6%N
  ; BI_local_get 6%N ; BI_local_get 3%N ; gt_u
  ; BI_if (BT_valtype None) [ BI_local_get 3%N ; BI_local_set 6%N ] []
  ; BI_block (BT_valtype None)
      [ BI_loop (BT_valtype None)
          [ BI_local_get 5%N ; BI_local_get 6%N
          ; BI_relop T_i32 (Relop_i (ROI_ge SX_U)) ; BI_br_if 1%N
          ; BI_local_get 4%N ; BI_local_get 5%N ; add
          ; BI_local_get 0%N ; BI_local_get 5%N ; add ; load_byte_u
          ; store_byte
          ; BI_local_get 5%N ; i32c 1 ; add ; BI_local_set 5%N
          ; BI_br 0%N ] ]
  ; BI_local_get 4%N ].

Definition cabi_realloc_func : module_func := {|
  modfunc_type := ty_realloc;
  modfunc_locals := repeat (T_num T_i32) 3;
  modfunc_body := cabi_realloc_body
|}.

(** ** The module

    Import order fixes the indices the bodies above use.  Note that the
    four globals are imported in CertiRocq's order even though [$cap] is never
    read: the adapter is merged into the backend's module, so the numbering has
    to agree. *)
Definition mut_i32 : global_type :=
  {| tg_mut := MUT_var; tg_t := T_num T_i32 |}.

Definition echo_adapter : module := {|
  mod_types :=
    [ Tf [T_num T_i32; T_num T_i32; T_num T_i32] []
    ; Tf [T_num T_i32; T_num T_i32] [T_num T_i32]
    ; Tf [T_num T_i32] [T_num T_i32]
    ; Tf [T_num T_i32; T_num T_i32] []
    ; Tf [T_num T_i32; T_num T_i32; T_num T_i32; T_num T_i32] [T_num T_i32] ];
  mod_funcs :=
    [ alloc_func; list_len_func; lower_func; lift_bytes_func
    ; some_func; ffi_echo_func; export_func; cabi_realloc_func ];
  mod_tables := [];
  mod_mems := [];
  mod_globals :=
    [ {| modglob_type := mut_i32; modglob_init := [ i32c 0 ] |} ];
  mod_elems := [];
  mod_datas := [];
  mod_start := None;
  mod_imports :=
    [ {| imp_module := String.list_byte_of_string "example:echo/echo@0.1.0"%string
       ; imp_name := String.list_byte_of_string "echo"%string
       ; imp_desc := MID_func ty_i32i32i32_void |}
    ; {| imp_module := String.list_byte_of_string "rocq"%string
       ; imp_name := String.list_byte_of_string "memory"%string
       ; imp_desc := MID_mem {| lim_min := 1%N; lim_max := None |} |}
    ; {| imp_module := String.list_byte_of_string "rocq"%string
       ; imp_name := String.list_byte_of_string "mem_ptr"%string
       ; imp_desc := MID_global mut_i32 |}
    ; {| imp_module := String.list_byte_of_string "rocq"%string
       ; imp_name := String.list_byte_of_string "cap"%string
       ; imp_desc := MID_global mut_i32 |}
    ; {| imp_module := String.list_byte_of_string "rocq"%string
       ; imp_name := String.list_byte_of_string "result"%string
       ; imp_desc := MID_global mut_i32 |}
    ; {| imp_module := String.list_byte_of_string "rocq"%string
       ; imp_name := String.list_byte_of_string "out_of_mem"%string
       ; imp_desc := MID_global mut_i32 |} ];
  mod_exports :=
    [ {| modexp_name := String.list_byte_of_string "ffi_echo"%string
       ; modexp_desc := MED_func fidx_export |}
    ; {| modexp_name := String.list_byte_of_string "cabi_realloc"%string
       ; modexp_desc := MED_func fidx_cabi_realloc |} ]
|}.
