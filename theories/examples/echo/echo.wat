;; Reference source for the M0 vertical slice (see PLAN-authentikate.md §4).
;;
;; One registered CertiRocq primitive
;;
;;   Axiom ffi_echo : bytes -> option bytes.
;;
;; realised against one WIT import
;;
;;   package example:echo@0.1.0;
;;   interface echo {
;;     variant error { unavailable }
;;     echo: func(data: list<u8>) -> result<list<u8>, error>;
;;   }
;;
;; The adapter is the smallest thing that still exercises the machinery the
;; authentikate adapters need: the bump allocator, bytestring lowering and
;; lifting (two of common.wat's loops), `option` construction, a canonical
;; return area with a variant discriminant, and the out-of-memory protocol.
;;
;; Canonical ABI return area for result<list<u8>, error>:
;;   disc @0 (1 byte, padded), ptr @4, len @8   -- 12 bytes, align 4.
;; `error` is payload-free with a single case, so the error arm carries nothing.
;;
;; `cabi_realloc` is exported because the component's `canon lower` for `echo`
;; needs a realloc in this instance: the answer is a `list<u8>`, and lowering
;; writes it into our memory.  Without the export the lowering cannot be formed.
;;
;; The heap helpers are inlined rather than expanded from authentikate's
;; common.wat, so this file validates on its own:
;;   wasm-tools validate theories/examples/echo/echo.wat
;; They are otherwise identical to the ones there.
;;
;; The Rocq counterpart of this file is echo_adapter.v; echo_correct.v states
;; what it means for it to implement `ffi_echo`.
(module
  ;; ---- from the CertiRocq module -------------------------------------------
  ;; All four globals are imported in CertiRocq's own order (mem_ptr, cap,
  ;; result, out_of_mem) so that global indices in this module agree with
  ;; glob_mem_ptr/glob_cap/glob_result/glob_out_of_mem. $cap is unused here but
  ;; imported anyway to keep the numbering.
  (import "rocq" "memory" (memory 1))
  (import "rocq" "mem_ptr" (global $mem_ptr (mut i32)))
  (import "rocq" "cap" (global $cap (mut i32)))
  (import "rocq" "result" (global $result (mut i32)))
  (import "rocq" "out_of_mem" (global $out_of_mem (mut i32)))

  ;; ---- from the echo provider ---------------------------------------------
  (import "example:echo/echo@0.1.0" "echo"
    (func $echo
      (param i32 i32)   ;; data: ptr, len
      (param i32)))     ;; return area

  (global $buf_len (mut i32) (i32.const 0))

  ;; ---- heap helpers --------------------------------------------------------

  ;; Bump allocation. On failure sets $out_of_mem; callers check the flag,
  ;; not the returned pointer.
  (func $alloc (param $n i32) (param $align i32) (result i32)
    (local $p i32) (local $end i32) (local $have i32) (local $pages i32)
    (local.set $p
      (i32.and
        (i32.add (global.get $mem_ptr) (i32.sub (local.get $align) (i32.const 1)))
        (i32.xor (i32.sub (local.get $align) (i32.const 1)) (i32.const -1))))
    (local.set $end (i32.add (local.get $p) (local.get $n)))
    (local.set $have (i32.shl (memory.size) (i32.const 16)))
    (if (i32.gt_u (local.get $end) (local.get $have))
      (then
        (local.set $pages
          (i32.add
            (i32.shr_u
              (i32.sub (local.get $end) (local.get $have))
              (i32.const 16))
            (i32.const 1)))
        (if (i32.eq (memory.grow (local.get $pages)) (i32.const -1))
          (then
            (global.set $out_of_mem (i32.const 1))
            (return (i32.const 0))))))
    (global.set $mem_ptr (local.get $end))
    (local.get $p))

  ;; No freeing: resizing allocates afresh and copies min(old_size, new_size).
  (func $cabi_realloc (export "cabi_realloc")
        (param $old_ptr i32) (param $old_size i32)
        (param $align i32) (param $new_size i32) (result i32)
    (local $p i32) (local $i i32) (local $n i32)
    (local.set $p (call $alloc (local.get $new_size) (local.get $align)))
    (if (global.get $out_of_mem) (then (return (i32.const 0))))
    (local.set $n (local.get $old_size))
    (if (i32.gt_u (local.get $n) (local.get $new_size))
      (then (local.set $n (local.get $new_size))))
    (block $done
      (loop $next
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
        (i32.store8
          (i32.add (local.get $p) (local.get $i))
          (i32.load8_u (i32.add (local.get $old_ptr) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $next)))
    (local.get $p))

  ;; EmptyString is immediate 1; String cells are [tag, byte, tail].
  (func $list_len (param $l i32) (result i32)
    (local $n i32)
    (block $done
      (loop $next
        (br_if $done (i32.and (local.get $l) (i32.const 1)))
        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (local.set $l (i32.load offset=8 (local.get $l)))
        (br $next)))
    (local.get $n))

  ;; Flatten bytes into fresh memory, leaving the length in $buf_len.
  (func $lower (param $l i32) (result i32)
    (local $len i32) (local $p i32) (local $i i32)
    (local.set $len (call $list_len (local.get $l)))
    (global.set $buf_len (local.get $len))
    (local.set $p (call $alloc (local.get $len) (i32.const 1)))
    (if (global.get $out_of_mem) (then (return (i32.const 0))))
    (block $done
      (loop $next
        (br_if $done (i32.and (local.get $l) (i32.const 1)))
        (i32.store8
          (i32.add (local.get $p) (local.get $i))
          (i32.shr_u (i32.load offset=4 (local.get $l)) (i32.const 1)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (local.set $l (i32.load offset=8 (local.get $l)))
        (br $next)))
    (local.get $p))

  ;; Build back to front so each cell's tail is already in hand.
  (func $lift_bytes (param $p i32) (param $len i32) (result i32)
    (local $acc i32) (local $cell i32)
    (local.set $acc (i32.const 1))  ;; EmptyString
    (block $done
      (loop $next
        (br_if $done (i32.eqz (local.get $len)))
        (local.set $len (i32.sub (local.get $len) (i32.const 1)))
        (local.set $cell (call $alloc (i32.const 12) (i32.const 4)))
        (if (global.get $out_of_mem) (then (return (i32.const 1))))
        (i32.store (local.get $cell) (i32.const 0))
        (i32.store offset=4 (local.get $cell)
          (i32.or
            (i32.shl
              (i32.load8_u (i32.add (local.get $p) (local.get $len)))
              (i32.const 1))
            (i32.const 1)))
        (i32.store offset=8 (local.get $cell) (local.get $acc))
        (local.set $acc (local.get $cell))
        (br $next)))
    (local.get $acc))

  (func $some (param $x i32) (result i32)
    (local $cell i32)
    (local.set $cell (call $alloc (i32.const 8) (i32.const 4)))
    (if (global.get $out_of_mem) (then (return (i32.const 1))))
    (i32.store (local.get $cell) (i32.const 0))
    (i32.store offset=4 (local.get $cell) (local.get $x))
    (local.get $cell))

  ;; ---- the registered primitive --------------------------------------------

  ;; ffi_echo : bytes -> option bytes
  ;;
  ;; `None` is the provider refusing; there is no "absent" case to confuse it
  ;; with, which is what makes this the easy slice. Out of memory also answers
  ;; `None` rather than trapping, exactly as the authentikate adapters do: the
  ;; immediate 1 is both `None` and the cheapest value to produce when there is
  ;; no memory left to produce anything else.
  (func $ffi_echo (param $arg i32) (result i32)
    (local $p i32) (local $len i32) (local $ret i32)

    (local.set $ret (call $alloc (i32.const 12) (i32.const 4)))
    (if (global.get $out_of_mem) (then (return (i32.const 1))))

    (local.set $p (call $lower (local.get $arg)))
    (local.set $len (global.get $buf_len))
    (if (global.get $out_of_mem) (then (return (i32.const 1))))

    (call $echo (local.get $p) (local.get $len) (local.get $ret))

    (if (result i32) (i32.load8_u (local.get $ret))
      (then (i32.const 1))
      (else
        (call $some
          (call $lift_bytes
            (i32.load offset=4 (local.get $ret))
            (i32.load offset=8 (local.get $ret)))))))

  ;; Registered primitives return void and publish through the result global.
  ;; The leading $force argument is CertiRocq 6d34b50b's lazy-force dummy.
  (func (export "ffi_echo") (param $force i32) (param $arg i32)
    (global.set $result (call $ffi_echo (local.get $arg))))
)
