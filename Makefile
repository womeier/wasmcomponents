ROCQMAKEFILE ?= Makefile.rocq

all: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE)

$(ROCQMAKEFILE): _RocqProject
	rocq makefile -f _RocqProject -o $(ROCQMAKEFILE)

clean: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) clean
	rm -f $(ROCQMAKEFILE) $(ROCQMAKEFILE).conf

install: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) install

.PHONY: all clean install
