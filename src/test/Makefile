TESTS_ML = $(wildcard *.ml)
TESTS = $(TESTS_ML:.ml=)
OCAML_CFLAGS = -I ../lang -I ../stream -I ../converters -I ../converters/audio/ -I ../tools  -I ../decoder -I .. $(liquidsoap_ocamlcflags) -thread
OCAML_LFLAGS = $(liquidsoap_ocamllflags) \
		extralib.cmx stringView.cmx strings.cmx utils.cmx log.cmx tutils.cmx \
		file_watcher_mtime.cmx dyntools.cmx JSON.cmx configure.cmx http.cmx id3v2.cmx \
		stream/frame.cmx avi.cmx vFrame.cmx aFrame.cmx generator.cmx doc.cmx plug.cmx \
                audio_converter.cmx video_converter.cmx decoder_utils.cmx decoder.cmx \
		lang_types.cmx pool.cmx request.cmx server.cmx source.cmx ../tools/unix_c.o

all: $(TESTS)

test.mp3:
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 -metadata title="Liquidsong" -metadata artist="Liquidsinger" test.mp3 ; fi

test: test.mp3 all
	@for t in $(TESTS); do echo "Testing $$t..."; ./$$t || exit 1; done

clean:
	rm -f $(TESTS) *.annot *.cmi *.cmx *.cmo *.o

%: %.ml
	@echo "Generating $@..."
	@$(OCAMLC) $(OCAML_CFLAGS) $(OCAML_LFLAGS) $^ -o $@

include ../../Makefile.defs
