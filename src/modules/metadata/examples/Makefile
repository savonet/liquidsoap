META = dune exec -- ./meta.exe

all: test id3v2

test:
	@dune test
	@dune build @citest

id3v2:
	@for i in id3v2/*.mp3; do \
	  echo; echo Testing $$i; echo; \
	  $(META) -b "$$i"; \
	done

test-mp3:
	@for i in ~/Music/misc/*; do \
	  echo; echo Testing $$i; echo; \
	  $(META) -b "$$i"; \
	done

.PHONY: id3v2
