# Enigma Assignment Starter

This starter project is a self-contained reconstruction for the adapted
CS 3110 Enigma assignment. It does not require Cornell CMS, Cornell GitHub,
Ugclinux, or Cornell's private Makefile/check scripts.

## Prerequisites

Install OCaml through opam, then install Dune and OUnit2:

```sh
opam install dune ounit2
eval $(opam env)
```

## Commands

```sh
dune build
dune test
dune clean
```

The `enigma.mli` file fixes the public names and types of the required
functions. If you accidentally change a required signature, `dune build`
will report the mismatch.

Begin with `index`, add a failing test in `enigma_test.ml`, then implement
the function and rerun `dune test`.
