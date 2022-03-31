all:
	@dune test

test-mp3:
	@for i in ~/Music/misc/*; do \
	  echo; echo Testing $$i; echo; \
	  dune exec -- meta "$$i"; \
	done
