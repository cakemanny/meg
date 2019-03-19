
SRCS := actions.peg \
	any.peg \
	capture.peg \
	classes.peg \
	literals.peg \
	peek.peg \
	repeats.peg \
	sequences.peg

TARGETS := $(SRCS:.peg=.byte)

PACKAGES := ppx_deriving.std,ppx_deriving.show

test: $(TARGETS)
	$(foreach target,$(TARGETS),./$(target);)

%.byte: %.ml
	ocamlfind ocamlc -linkpkg -package $(PACKAGES) -o $@ $<

%.ml: %.peg
	dune exec meg $< > $@

.PHONY: clean

clean:
	rm -Rf *.ml *.byte *.cmi *.cmo
