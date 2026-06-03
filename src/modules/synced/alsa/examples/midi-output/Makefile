all: build

test: build
	@OCAMLRUNPARAM=b dune exec ./output.exe

build:
	@dune build

clean:
	@dune clean
