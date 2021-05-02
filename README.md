# meg

meg is mostly an OCaml implementation of a
[PEG](https://pdos.csail.mit.edu/papers/parsing%3Apopl04.pdf) based parser
generator.
It's based heavily on \_why's [greg](https://github.com/whymirror/greg)

## Installation
```
opam install .
```

## Example use
```
meg samples/desk_calc.peg > samples/desk_calc.ml
ocamlc -o samples/desk_calc samples/desk_calc.ml
```

## Hacking
To install the dependencies and test dependencies without installing `meg`:
```
opam install . --deps-only --with-test
```

Then building and testing can be run with dune:
```
dune build
dune runtest
dune exec meg ...
```

## Links

  * [OOC Lang greg](https://github.com/ooc-lang/greg)
  * [Ian Piumarta's peg/leg](http://piumarta.com/software/peg/)
  * [2004 PEG paper](http://pdos.csail.mit.edu/papers/parsing%3Apopl04.pdf)

## License

peg/leg is copyright (c) 2007 by Ian Piumarta released under an MIT license.
As is greg. As is meg.
