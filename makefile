
OCB = ocamlbuild
OCBFLAGS = -use-ocamlfind -use-menhir -I src

.PHONY: all clean sanity native byte debug clean_tests test

all: byte

native: sanity clean_tests
	$(OCB) $(OCBFLAGS) meg.native

byte: sanity clean_tests
	$(OCB) $(OCBFLAGS) meg.byte

debug: sanity clean_tests
	$(OCB) $(OCBFLAGS) -tag debug meg.byte

sanity:
	which menhir

clean: clean_tests
	$(OCB) $(OCBFLAGS) -clean

clean_tests:
	$(MAKE) -C tests clean

test:
	$(MAKE) -C tests test
