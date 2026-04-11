ROCQMAKEFILE ?= Makefile.rocq

all: rocq

$(ROCQMAKEFILE): _RocqProject
	rocq makefile -f _RocqProject -o $(ROCQMAKEFILE)

rocq: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE)

# ============================================================
# OCaml binary from extracted Rocq code
# ============================================================

src/extraction: rocq
	rm -rf src/extraction theories/extraction theories/extraction.vo
	make -f $(ROCQMAKEFILE)

src/wc-tools: src/extraction src/main.ml
	MODULES="$$(cd src/extraction && ocamldep -sort -one-line *.mli)"; \
	cd src/extraction && \
		ocamlopt $$(for mod in $$MODULES; do mod=$${mod%.mli}; printf '%s.mli %s.ml ' "$$mod" "$$mod"; done) && \
	cd ../.. && \
	ocamlopt \
		-I src/extraction \
		$$(for mod in $$MODULES; do mod=$${mod%.mli}; printf 'src/extraction/%s.cmx ' "$$mod"; done) \
		src/main.ml \
		-o src/wc-tools




clean: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) clean
	rm -f $(ROCQMAKEFILE) $(ROCQMAKEFILE).conf
	rm -rf src/extraction docs
	rm -f wc-tools src/wc-tools
	find src -name "*.cmi" -delete
	find src -name "*.cmx" -delete
	find src -name "*.o" -delete

install: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) install

docs: rocq
	mkdir -p docs
	rocq doc --html --toc --utf8 \
		-R theories wasmcomponents \
		-d docs \
		theories/wit_ast.v theories/wit_parser.v

docs-run:
	cd docs && python3 -m http.server -b 127.0.0.1

run_tests: src/wc-tools
	python3 testing/test_parser.py

run_tests_components: src/wc-tools
	python3 testing/test_component_parser.py

.PHONY: all rocq clean install docs run_tests run_tests_components
