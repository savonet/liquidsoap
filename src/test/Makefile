TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)

all: $(TESTS)

SUBDIRS=""
top_srcdir=../..
include $(top_srcdir)/src/Makefile

test.mp3:
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 -metadata title="Liquidsong" -metadata artist="Liquidsinger" test.mp3 ; fi

test: test.mp3 all
	@for t in $(TESTS); do echo "Testing $$t..."; ./$$t || exit 1; done

clean:
	rm -f $(TESTS) *.annot *.cm* *.o

all-subdirs: ;
all-auto-ocaml-prog: ;

ml = $(subst runner.ml,,$(subst main.ml,,$(liquidsoap_sources)))
cmx = $(ml:.ml=.$(o))

%: %.ml
	@echo "Generating $@..."
	@cd .. && $(OCAMLC) $(liquidsoap_ocamlcflags) $(OCAML_CFLAGS) $(liquidsoap_ocamllflags) $(OCAML_LFLAGS) -c test/$^
	@cd .. && $(OCAMLC) $(liquidsoap_ocamlcflags) $(OCAML_CFLAGS) $(liquidsoap_ocamllflags) $(OCAML_LFLAGS) $(cmx) $(liquidsoap_c_files:.c=.o) test/$(patsubst %.ml,%.$(o),$^) -o test/$@

export OCAMLPATH := $(OCAMLPATH)
