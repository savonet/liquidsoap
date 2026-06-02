all: build

build:
	@dune build

test: build
	@export OCAMLRUNPARAM=b && dune exec ./sine.exe
