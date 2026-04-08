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

src/wit_parser_bin: src/extraction src/main.ml
	cd src/extraction && ocamlopt \
		BinNums.mli BinNums.ml \
		Datatypes.mli Datatypes.ml \
		Nat.mli Nat.ml \
		BinPos.mli BinPos.ml \
		PosDef.mli PosDef.ml \
		BinNat.mli BinNat.ml \
		Ascii.mli Ascii.ml \
		String.mli String.ml \
		ListDef.mli ListDef.ml \
		List.mli List.ml \
		wit_ast.mli wit_ast.ml \
		wit_parser.mli wit_parser.ml

	ocamlopt \
		-I src/extraction \
		src/extraction/BinNums.cmx \
		src/extraction/Datatypes.cmx \
		src/extraction/Nat.cmx \
		src/extraction/BinPos.cmx \
		src/extraction/PosDef.cmx \
		src/extraction/BinNat.cmx \
		src/extraction/Ascii.cmx \
		src/extraction/String.cmx \
		src/extraction/ListDef.cmx \
		src/extraction/List.cmx \
		src/extraction/wit_ast.cmx \
		src/extraction/wit_parser.cmx \
		src/main.ml \
		-o src/wit_parser_bin

clean: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) clean
	rm -f $(ROCQMAKEFILE) $(ROCQMAKEFILE).conf
	rm -rf src/extraction docs
	rm -f wit_parser_bin
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

run_tests: src/wit_parser_bin
	python3 testing/test_parser.py

.PHONY: all rocq clean install docs run_tests
