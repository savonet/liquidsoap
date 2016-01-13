TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)
OCAML_CFLAGS = -I ../tools -I .. $(liquidsoap_ocamlcflags) -thread
OCAML_LFLAGS = $(liquidsoap_ocamllflags) \
		stdlib.cmx utils.cmx dyntools.cmx shutdown.cmx tutils.cmx \
		file_watcher_mtime.cmx configure.cmx http.cmx JSON.cmx \
		../tools/locale_c.o

all: $(TESTS)

test: all
	@for t in $(TESTS); do echo "Testing $$t..."; ./$$t; done

clean:
	rm -f $(TESTS) *.annot *.cmi *.cmx *.cmo *.o

%: %.ml
	@echo "Generating $@..."
	@$(OCAMLC) $(OCAML_CFLAGS) $(OCAML_LFLAGS) $^ -o $@

include ../../Makefile.defs
