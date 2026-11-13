ROCQMAKEFILE ?= Makefile.rocq

all: rocq

$(ROCQMAKEFILE): _RocqProject
	rocq makefile -f _RocqProject -o $(ROCQMAKEFILE)

rocq: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE)

install: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) install

clean: $(ROCQMAKEFILE)
	$(MAKE) -f $(ROCQMAKEFILE) clean
	rm -f $(ROCQMAKEFILE) $(ROCQMAKEFILE).conf

.PHONY: all rocq install clean
