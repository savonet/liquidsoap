MD = $(wildcard content/*.md)

all:
	@dune build @doc

test:
	@for i in $(MD); do \
	echo -n "Compiling $$i... "; \
	pandoc $$i -t json | pandoc-include --directory "content/liq" | pandoc -f json --metadata title="bla" --template=template.html -o /tmp/`basename $$i .md`.html; \
	echo "done"; \
	done
	@dune build @doctest
