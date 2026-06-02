all: build

build:
	@dune build

test:
	@dune exec ./devices.exe

gdb: build
	gdb ../../_build/default/examples/devices/devices.exe
