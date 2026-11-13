# WasmGlue

The reusable half of this repository's adapter verification, as a package
of its own (`rocq-wasmcomponents-certirocq`, logical path `WasmGlue`).  It
knows nothing about the component-model formalization in `../theories` —
only about core Wasm, CertiRocq's calling convention, and the glue code
`componentize-formal` emits.  The `wasmcomponents-` in the package name
is a namespace, not a dependency: `rocq-wasmcomponents` requires this
package, never the reverse, and this one is installable on its own.

The logical path is `WasmGlue` rather than something under
`wasmcomponents.`, because `-Q theories wasmcomponents` in the parent
already claims that root — two packages installing into
`user-contrib/wasmcomponents/` would collide.

| module               | what is in it                                                                |
| -------------------- | ---------------------------------------------------------------------------- |
| `wasm_mem.v`         | `store`/`load` on a `meminst`: existence, length, framing, disjointness       |
| `wasm_steps.v`       | the `dostep_*` tactics and the `reduce_trans` congruences for frames/labels   |
| `certirocq_heap.v`   | CertiRocq's value representation: `repr_prim`, closures, `mem_agree_below`    |
| `certirocq_inv.v`    | `adapter_inv` and the store, global and arithmetic specs stated over it       |
| `certirocq_adapter.v`| the adapter fragments the generator emits, with their specs                   |

`certirocq_adapter.v` is the one to read first.  It has three layers:

- the instruction shorthands (`i32c`, `load_word`, `store_i64_at`, ...)
  that keep an emitted body diffable against its `.wat`;
- the fragments the generator emits for *every* world, parameterised by
  the local slots and sizes it chose — `alloc_scratch`, `room_check`,
  `fail_with`, `oom_guard`;
- `scratch_ok`, the layout those fragments establish, plus the arithmetic
  and memory lemmas an adapter's proof needs.  `scratch_ok` is why a
  lowering step's hypotheses are one record rather than five unexplained
  inequalities: `alloc_scratch` produces it, each store consumes it, and
  the disjointness facts come out of it as lemmas (`sk_box_below`,
  `sk_arg_mod`, ...).

What is *not* here is lowering and lifting a particular WIT type into
the canonical ABI: that is generated per type and proven per type, so it
belongs with the world that uses it until there are enough worlds to see
the shape of that layer.  `theories/examples/id63` is the first one.

## Building

```sh
make                 # needs rocq and coq-wasm on the path
make install         # into $COQLIBINSTALL, or Rocq's user-contrib
```

The parent project depends on this one as an installed package.  For
in-tree development the dev shell puts this directory on `ROCQPATH`, so
`make` here is enough to make `From WasmGlue Require Import ...` resolve
from the working tree.
