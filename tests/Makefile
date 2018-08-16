
SRCS := character_classes.ml literals.ml repeats.ml
TARGETS := $(SRCS:.ml=.byte)

test: $(TARGETS)
	./character_classes
	./literals
	./repeats

%.byte: %.ml
	ocamlc -o $@ $<

%.ml: %.peg
	../meg.byte $< > $@
