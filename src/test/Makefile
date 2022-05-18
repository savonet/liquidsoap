TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)

all: $(TESTS)

test.mp3:
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 -metadata title="Liquidsong" -metadata artist="Liquidsinger" test.mp3 ; fi

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
	cd json && find . -maxdepth 1 -type d | grep -v '^\.$$' | xargs rm -rf

json5:
	git clone --depth 1 https://github.com/json5/json5-tests.git json5
	cd json5 && find . -type f | egrep '\.json|\.js|\.txt' | xargs -I % mv % .
	cd json5 && find . -type d | grep -v '^\.$$' | xargs rm -rf

lib:
	@$(MAKE) -C .. $@

include ../../Makefile.defs

INCDIRS=. playlists converters decoder encoder_formats lang lang_encoders stream tools

%: %.ml ../liquidsoap.$(a)
	@echo "Generating $@..."
	@cd .. && $(OCAMLC) -thread -linkall -o test/$@ $(liquidsoap_ocamlcflags) $(liquidsoap_ocamllflags) $(patsubst %,-I %,$(INCDIRS)) liquidsoap.$(a) test/$<

../liquidsoap.$(a):
	@$(MAKE) -C .. lib

export OCAMLPATH := $(OCAMLPATH)
