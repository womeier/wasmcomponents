WIP formalisation of the Wasm component standard

Most of the code is written by LLMs and is experimental, it is not intended to be used by others.

## Layout

`theories/` is the component-model formalization and the verified
adapters built on it.  `wasmglue/` is a separate package,
`rocq-wasmcomponents-certirocq` (logical path `WasmGlue`): the glue layer —
CertiRocq's calling convention and the adapter code
`componentize-formal` emits — which depends on nothing in `theories/`.
See `wasmglue/README.md`.

The dev shell puts `wasmglue/` on `ROCQPATH`, so `make -C wasmglue`
makes changes to it visible. Note that the generated `Makefile.rocq`
tracks dependencies only inside `theories/`: after rebuilding the
library, `make` will report nothing to do and leave stale `.vo` files
that fail with *inconsistent assumptions*. Delete `theories/**/*.vo`
and rebuild when the library changes.

## Usage
```
# build packages
nix build .#wasmcomponents
nix build .#wasmglue

# enter dev shell
nix develop

# build, in the dev shell
make -C wasmglue
rocq makefile -f _RocqProject -o Makefile.rocq && make -f Makefile.rocq -j4

# enter shell with package
nix develop .#test-shell
```

## Resources (see the repos in the submodules folder)
The [documentation](https://component-model.bytecodealliance.org/introduction.html) of the component model by the Bytecodealliance.

The [reference implementation](https://github.com/bytecodealliance/wasm-tools) of components in Rust,
in particular the folders `crates/wit-*`.

The [documentation](https://github.com/WebAssembly/component-model) of the Wasm component standard.

The official [documentation](https://github.com/WebAssembly/WASI) of WASI, see in particular `docs/Proposals.md` for an overview.

The [Rocq formalization](https://github.com/wasmCert/WasmCert-Coq/) of the core Wasm standard.
We want to reuse as many definitions as possible, for e.g. types etc.
