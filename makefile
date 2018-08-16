
OCB = ocamlbuild
OCBFLAGS = -use-ocamlfind -use-menhir -I src

.PHONY: all clean sanity native byte debug

all: byte

native: sanity
	$(OCB) $(OCBFLAGS) meg.native

byte: sanity
	$(OCB) $(OCBFLAGS) meg.byte

debug: sanity
	$(OCB) $(OCBFLAGS) -tag debug meg.byte

sanity:
	which menhir

clean:
	$(OCB) $(OCBFLAGS) -clean

