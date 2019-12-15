TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)
OCAML_CFLAGS = -I ../tools -I .. $(liquidsoap_ocamlcflags) -thread
OCAML_LFLAGS = $(liquidsoap_ocamllflags) \
		extralib.cmx stringView.cmx strings.cmx utils.cmx log.cmx tutils.cmx \
		file_watcher_mtime.cmx dyntools.cmx configure.cmx http.cmx JSON.cmx id3v2.cmx \
		stream/frame.cmx avi.cmx \
		../tools/unix_c.o

all: $(TESTS)

test: all
	@for t in $(TESTS); do echo "Testing $$t..."; ./$$t || exit 1; done

clean:
	rm -f $(TESTS) *.annot *.cmi *.cmx *.cmo *.o

%: %.ml
	@echo "Generating $@..."
	@$(OCAMLC) $(OCAML_CFLAGS) $(OCAML_LFLAGS) $^ -o $@

include ../../Makefile.defs
