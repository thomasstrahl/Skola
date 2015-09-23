include ../../../Mercury.options.common

ZINC=$(INSTALL_PREFIX)/bin/zinc$(EXEEXT)

MZN_MODELS=$(wildcard *.mzn)
FZN_FILES=$(MZN_MODELS:.mzn=.fzn)
OZN_FILES=$(OZN_MODELS:.mzn=.ozn)
ZINC_EXECS=$(MZN_MODELS:.mzn=)

.PHONY: realclean
realclean:
	$(RM) -f $(FZN_FILES) $(OZN_FILES) $(ZINC_EXECS)
	$(RM) -f *.out *.res *.err FAILED_TESTS
	$(RM) -f *.cdo *.cds *.diff *.diff[0-9] zm*.mh zm*.m
	$(RM) -f Prof.Decl Prof.CallPair Prof.MemoryCells Prof.MemoryWords Deep.data
	$(RM) -rf Cadmium Mercury

.PHONY: distclean
distclean: realclean
