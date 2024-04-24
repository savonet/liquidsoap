MD = $(wildcard content/*.md)

all: dune
	@dune build @doc

test: dune
	@for i in $(MD); do \
	echo -n "Compiling $$i... "; \
	pandoc $$i -t json | pandoc-include --directory "content/liq" | pandoc -f json --metadata title="bla" --template=template.html -o /tmp/`basename $$i .md`.html; \
	echo "done"; \
	done
	@dune build @doctest

dune:
	@dune build @gendune --auto-promote

.PHONY: dune
