all: dune
	@dune build @doc

test: dune
	@dune build @doctest

dune:
	@dune build @gendune --auto-promote

.PHONY: dune
