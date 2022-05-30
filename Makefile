all: build

build install clean test:
	@dune $@
