all: build

build:
	@dune build

autotune:
	@dune exec ./autotune.exe

dictee:
	@dune exec ./dictee.exe test.mp3

display:
	@dune exec ./display.exe test.ppm

drums:
	@dune exec ./drums.exe

fft:
	@dune exec ./fft.exe test.wav

id:
	@dune exec ./id.exe test.wav

midiplayer:
	@dune exec ./midiplayer.exe test.mid

sine_wav:
	@dune exec ./sine_wav.exe

graphics:
	@dune exec ./graphics_test.exe

test:
	@dune exec ./test.exe

quick-test:
	@dune exec ./test.exe -- --skip-long

valgrind: build
	valgrind ../_build/default/examples/test.exe --skip-long
