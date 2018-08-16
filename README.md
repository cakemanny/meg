# meg

meg is mostly an OCaml implementation of a
[PEG](https://pdos.csail.mit.edu/papers/parsing%3Apopl04.pdf) based parser
generator.
It's based heavily on \_why's [greg](https://github.com/whymirror/greg)

## Requirements

  * ocaml
  * ocamlbuild
  * Make

## Required Packages

These can be installed using `opam` or any other way seen fit

  * Menhir
  * Stringext


## Build instructions

Run `make native` or `make byte` for the native and bytecode versions
respectively.


## Links

  * [OOC Lang greg](https://github.com/ooc-lang/greg)
  * [Ian Piumarta's peg/leg](http://piumarta.com/software/peg/)
  * [2004 PEG paper](http://pdos.csail.mit.edu/papers/parsing%3Apopl04.pdf)

## License

peg/leg is copyright (c) 2007 by Ian Piumarta released under an MIT license.
As is greg. As is meg.
