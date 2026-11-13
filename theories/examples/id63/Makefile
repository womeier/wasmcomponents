# componentize-formal generate wrote this from the world `wrapper-world` and
# component.toml.
#
# `make` assembles the component; `make run ARGS=...` invokes the export.
#
# Everything here is derived from the world and the config -- the file
# names, the export name -- bar one assumption worth naming: that the
# module CertiRocq emits exports its function table, which is what the
# adapter links the compiled closures through. The first rule below
# checks it rather than letting the link fail (TODO.md#table-export).

WASM_NAME  := rocq-id63
COMPONENT  := $(WASM_NAME).comp.wasm
WIT        := wit
WIT_FILES  := $(wildcard $(WIT)/*.wit) $(wildcard $(WIT)/deps/*/*.wit)
EXPORT     := id63
# Reference types: the adapter imports the backend's function table.
# Tail calls: the backend emits `return_call_indirect` in the
# wrappers it generates around a closure body, and binaryen
# validates the input it is given.
MERGE_OPTS := --enable-reference-types --enable-tail-call
# `make run ARGS=8` invokes the export with 8.
ARGS       ?=

.PHONY: all run wit clean
all: $(COMPONENT)

run: $(COMPONENT)
	wasmtime run --invoke '$(EXPORT)($(ARGS))' $<

# The component's own view of itself, which is the world it was embedded with.
wit: $(COMPONENT)
	wasm-tools component wit $<

# ---------------------------------------------------------------------------
# The Rocq half.
%.vo: %.v
	rocq c $<

# impl.v implements the generated Module Type, so interface.vo comes first.
impl.vo: interface.vo

$(WASM_NAME).wasm: extraction.v impl.vo
	rocq c extraction.v

# ---------------------------------------------------------------------------
# The component half.
%.wasm: %.wat
	wasm-tools parse $< -o $@

# The adapter calls the compiled closures through the backend's table, so the
# export has to be there. A CertiRocq predating it fails here, with the reason,
# rather than at link time with a message about a missing symbol.
$(COMPONENT): $(WASM_NAME).wasm adapter.wasm $(WIT_FILES)
	@wasm-tools parse $(WASM_NAME).wasm -t \
	  | grep -q '(export "table"' \
	  || { echo 'error: this CertiRocq does not export its function table'; \
	       echo '       as "table", which the adapter links the compiled'; \
	       echo '       closures through. Move the CertiRocq you build against'; \
	       echo '       forward -- see TODO.md#table-export.'; \
	       exit 1; }
	wasm-merge $(WASM_NAME).wasm rocq adapter.wasm adapter $(MERGE_OPTS) -o merged.wasm
	wasm-tools component embed $(WIT) merged.wasm -o embedded.wasm
	wasm-tools component new embedded.wasm -o $@

clean:
	rm -f *.wasm *.vo *.vok *.vos *.glob .*.aux *.ir
