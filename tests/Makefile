
SRCS := classes.peg literals.peg repeats.peg actions.peg sequences.peg

TARGETS := $(SRCS:.peg=.byte)

test: $(TARGETS)
	$(foreach target,$(TARGETS),./$(target);)

%.byte: %.ml
	ocamlc -o $@ $<

%.ml: %.peg
	../meg.byte $< > $@

.PHONY: clean

clean:
	rm -Rf *.ml *.byte *.cmi *.cmo
