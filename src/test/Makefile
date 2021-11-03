TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)

all: $(TESTS)

SUBDIRS=""
top_srcdir=../..
include $(top_srcdir)/src/Makefile

test.mp3:
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=10" -ac 2 -metadata title="Liquidsong" -metadata artist="Liquidsinger" test.mp3 ; fi

test: test.mp3 json5 json all run_stream_decoder_test
	@for t in $(filter-out stream_decoder_test,$(TESTS)); do echo "Testing $$t..."; ./$$t || exit 1; done

clean:
	rm -f $(TESTS) *.annot *.cm* *.o bla.mp3 test.mp3
	rm -rf json5 json

run_stream_decoder_test: stream_decoder_test test.mp3
	./stream_decoder_test test.mp3 bla.mp3

json:
	git clone --depth 1 https://github.com/nst/JSONTestSuite json
	cd json && mv test_parsing/*.json .
	cd json && find . -type d -d 1 | grep -v '^\.$$' | xargs rm -rf

json5:
	git clone --depth 1 https://github.com/json5/json5-tests.git json5
	cd json5 && find . -type f | egrep '\.json|\.js|\.txt' | xargs -I % mv % .
	cd json5 && find . -type d | grep -v '^\.$$' | xargs rm -rf

all-subdirs: ;
all-auto-ocaml-prog: ;

ml = $(subst runner.ml,,$(subst main.ml,,$(liquidsoap_sources)))
cmx = $(ml:.ml=.$(o))

%: %.ml
	@echo "Generating $@..."
	@cd .. && $(OCAMLC) $(liquidsoap_ocamlcflags) $(OCAML_CFLAGS) $(liquidsoap_ocamllflags) $(OCAML_LFLAGS) -c test/$^
	@cd .. && $(OCAMLC) $(liquidsoap_ocamlcflags) $(OCAML_CFLAGS) $(liquidsoap_ocamllflags) $(OCAML_LFLAGS) $(cmx) $(liquidsoap_c_files:.c=.o) test/$(patsubst %.ml,%.$(o),$^) -o test/$@

export OCAMLPATH := $(OCAMLPATH)
