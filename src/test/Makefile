TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)
OCAML_CFLAGS = -I ../tools -I .. -I ../decoder $(liquidsoap_ocamlcflags) -thread
OCAML_LFLAGS = $(liquidsoap_ocamllflags) \
		extralib.cmx utils.cmx tools/log.cmx tutils.cmx \
		file_watcher_mtime.cmx tools/dyntools.cmx configure.cmx http.cmx JSON.cmx \
		stream/frame.cmx avi.cmx doc.cmx pool.cmx plug.cmx decoder.cmx request.cmx playlist_parser.cmx \
		../tools/unix_c.o

all: $(TESTS)

test: all
	@for t in $(TESTS); do echo "Testing $$t..."; ./$$t; done

clean:
	rm -f $(TESTS) *.annot *.cmi *.cmx *.cmo *.o

%: %.ml
	@echo "Generating $@..."
	@$(OCAMLC) $(OCAML_CFLAGS) $(OCAML_LFLAGS) $^ -o $@

include ../../Makefile.defs

export OCAMLPATH := $(OCAMLPATH)
