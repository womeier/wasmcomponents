ROCQMAKEFILE ?= Makefile.rocq

all: rocq

$(ROCQMAKEFILE): _RocqProject
	rocq makefile -f _RocqProject -o $(ROCQMAKEFILE)

rocq: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE)

# ============================================================
# OCaml binary from extracted Rocq code
# ============================================================

src/extraction: theories/extraction.vo
	rm -rf src/extraction
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
		src/extraction/List.cmx \
		src/extraction/wit_ast.cmx \
		src/extraction/wit_parser.cmx \
		src/main.ml \
		-o src/wit_parser_bin

clean: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) clean
	rm -f $(ROCQMAKEFILE) $(ROCQMAKEFILE).conf
	rm -rf src/extraction
	rm -f wit_parser_bin
	find src -name "*.cmi" -delete
	find src -name "*.cmx" -delete
	find src -name "*.o" -delete

install: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) install


run_tests: rocq src/wit_parser_bin
	src/wit_parser_bin --help

.PHONY: all rocq clean install
