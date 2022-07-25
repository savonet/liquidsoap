META = dune exec -- ./meta.exe

all: test id3v2

test:
	@dune test

id3v2:
	@for i in id3v2/*.tag; do \
	  echo; echo Testing $$i; echo; \
	  $(META) "$$i"; \
	done

test-mp3:
	@for i in ~/Music/misc/*; do \
	  echo; echo Testing $$i; echo; \
	  $(META) "$$i"; \
	done

.PHONY: id3v2
