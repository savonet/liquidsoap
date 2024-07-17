
SUBDIRS= operators sources tools formats protocols outputs io plugins stream \
		 visualization lang analyze playlists

DISTFILES = $(wildcard *.mli) Makefile \
	$(wildcard *.ml) META.in

ocaml_progs= liquidsoap

formats = \
	$(if $(W_ID3TAG),formats/id3tag_stubs.c formats/id3tag.ml) \
	$(if $(W_ID3TAG),formats/id3tag_plug.ml) \
	$(if $(W_MP3ID3),formats/mp3id3_plug.ml) \
	$(if $(W_MP3),formats/mp3.ml) \
	$(if $(W_VORBIS),formats/oggformat.ml)  \
	formats/wavformat.ml
#	formats/midiformat.ml

playlists = \
	playlists/playlist_basic.ml $(if $(W_XML),playlists/playlist_xml.ml)

protocols = \
	protocols/say.ml protocols/annotate.ml \
	protocols/mpd.ml protocols/extproto.ml \
	$(if $(W_XML),protocols/lastfm_req.ml)

sources = sources/generated.ml \
	sources/blank.ml sources/request_source.ml \
	sources/sine.ml sources/square.ml sources/saw.ml \
	sources/req_simple.ml sources/playlist.ml \
	sources/req_queue.ml sources/req_equeue.ml \
	sources/http_source.ml \
	$(if $(W_VORBIS),sources/http_vorbis.ml) \
	sources/noise.ml sources/harbor_input.ml \
	$(if $(W_MP3),sources/http_mp3.ml) \
	$(if $(W_FAAD),sources/http_aac.ml) \
	$(if $(W_ALSA),sources/alsa_in.ml) \
	$(if $(W_RTP),sources/rtp_input.ml) \
	$(if $(W_XML),sources/lastfm_input.ml)

operators = \
	operators/switch.ml operators/fade.ml operators/add.ml \
	operators/rewrite_metadata.ml operators/on_metadata.ml \
	operators/store_metadata.ml operators/on_track.ml \
	operators/insert_metadata.ml operators/id.ml \
	operators/delay.ml operators/setvol.ml \
	operators/normalize.ml operators/echo.ml \
	operators/prepend.ml operators/append.ml operators/accel.ml \
	operators/noblank.ml operators/compand.ml \
	operators/sequence.ml operators/cross.ml operators/smartcross.ml \
	operators/mixing_table.ml operators/filter.ml \
	operators/mean.ml operators/swap.ml \
	operators/pan.ml \
	operators/compress_exp.ml operators/pitch.ml \
	operators/fir_filter.ml operators/iir_filter.ml \
	operators/biquad_filter.ml \
	operators/map_op.ml operators/resample.ml \
	operators/flanger.ml operators/comb.ml \
	operators/compress.ml operators/clip.ml \
	$(if $(W_SOUNDTOUCH),operators/soundtouch_op.ml) \
	$(if $(W_SOUNDTOUCH),operators/st_bpm.ml) \
	$(if $(W_LADSPA),operators/ladspa_op.ml)
#	operators/pipe.ml

analyze = analyze/analyze_rms.ml

outputs = outputs/output.ml outputs/file_output.ml \
        $(if $(W_LAME),outputs/lame_encoded.ml) \
        $(if $(W_VORBIS), outputs/vorbis_encoded.ml)  \
        $(if $(W_SHOUT),outputs/icecast2.ml \
	$(if $(W_VORBIS), outputs/vorbis_encoded_shout.ml) \
	$(if $(W_LAME),outputs/lame_encoded_shout.ml) ) \
	outputs/wavfile.ml \
	$(if $(W_AO),outputs/ao_out.ml) \
	$(if $(W_FAAC),outputs/faac_encoded.ml) \
	$(if $(W_ALSA),outputs/alsa_out.ml) \
	$(if $(W_RTP),outputs/rtp_output.ml)

io = $(if $(W_OSS),io/oss_io_c.c io/oss_io.ml) \
	$(if $(W_ALSA),io/alsa_io.ml) \
	$(if $(W_PORTAUDIO),io/portaudio_io.ml) \
	$(if $(W_JACK),io/jack_io.ml)

tools = tools/doc.ml tools/plug.ml tools/utils.ml \
	tools/tutils.ml tools/rqueue.ml tools/ringbuffer.ml \
	tools/wav.ml $(if $(BYTE),tools/dynliq.ml) configure.ml \
	tools/sutils.ml tools/http.ml \
	$(if $(W_XML),tools/xmliq.ml tools/lastfm.ml)

stream = stream/midi.ml stream/frame.ml fmt.ml \
	stream/float_pcm_c.c stream/float_pcm.ml

visualization = visualization/vumeter.ml \
	$(if $(W_GRAPHICS),visualization/vis_volume.ml) \
	$(if $(W_GL),visualization/GLvisualizer.ml) \
	$(if $(W_GL),visualization/vis_glvolume.ml)

liquidsoap_sources= $(tools) SVN.ml $(stream) stream/aFrame.ml \
	$(if $(W_RTP),tools/rtp_c.o tools/rtp.ml) \
	decoder.ml playlist_parser.ml request.ml source.ml root.ml \
	tools/server.ml tools/server_builtins.ml \
	lang/lang_types.ml lang/lang_values.ml \
	lang/lang_lexer.ml lang/lang_parser.ml lang/lang_pp.ml lang/lang.ml \
	tools/harbor.ml \
	$(formats) $(protocols) $(sources) $(operators) $(outputs) $(io) \
	$(analyze) $(playlists) $(visualization) \
	shebang.ml \
	lang/lang_builtins.ml \
	main.ml

OCAML_CFLAGS= -thread \
	-I tools -I lang -I playlists -I stream -I outputs -I formats \
	-I sources -I operators -I io -I visualization -I analyze
OCAML_LFLAGS= $(if $(BYTE),dynlink.cma) $(if $(W_RTP),-cclib -lortp) \
	$(if $(W_ID3TAG),-cclib -lid3tag)
DEP_OPTS= -I sources -I operators -I tools -I lang -I formats -I playlists \
		  -I outputs -I io -I stream -I visualization -I analyze

liquidsoap_doc_sources= \
	source.ml root.ml \
	tools/plug.ml \
	decoder.ml request.ml outputs/output.ml \
	lang/lang.ml tools/tutils.ml \
	tools/doc.ml tools/wav.ml \
	$(if $(W_RTP),tools/rtp.ml) \
	sources/request_source.ml operators/switch.ml \
	configure.ml fmt.ml \
	stream/frame.ml stream/float_pcm.ml stream/aFrame.ml

top_srcdir=..
include $(top_srcdir)/Makefile.rules

tools/rtp_c.o: tools/rtp_c.c
	$(MAKE) -C tools rtp_c.o
formats/id3tag_stubs.o: formats/id3tag_stubs.c
	$(MAKE) -C formats id3tag_stubs.o
stream/float_pcm_c.o: stream/float_pcm_c.c
	$(MAKE) -C stream float_pcm_c.o
io/oss_io_c.o: io/oss_io_c.c
	$(MAKE) -C io oss_io_c.o

# Compute something like " (trunk/liquidsoap@rev1234)" when .svn is present,
SVN.ml: Makefile $(liquidsoap_sources:SVN.ml=)
	if test -d .svn && which svn > /dev/null ; then \
	  url=`LC_ALL=C svn info | grep URL | \
	       sed -e 's:.*svnroot/savonet/\(.*\)/src:\1:'`; \
	  revision=`LC_ALL=C svn info | grep Revision | \
	            sed -e 's/Revision: \(.*\)/\1/'`  ; \
	  echo "let rev = \" ($$url@rev$$revision)\"" > SVN.ml ; \
	else \
	  echo "let rev = \"\"" > SVN.ml ; \
	fi

# Dirty hack since ocamlcp does not play nicely with include
profile:
	-$(MAKE)
	cd lang; ocamlc -c -I .. -I ../tools lang_values.ml
	$(MAKE)

libliq_sources= $(tools) SVN.ml $(stream) stream/aFrame.ml \
	shebang.ml playlist_parser.ml \
	$(if $(W_RTP),tools/rtp_c.o tools/rtp.ml) \
	decoder.ml request.ml source.ml root.ml \
	tools/server.ml tools/server_builtins.ml \
	lang/lang_types.ml lang/lang_values.ml \
	lang/lang_lexer.ml lang/lang_parser.ml lang/lang_pp.ml lang/lang.ml \
	tools/harbor.ml \
	$(formats) $(protocols) $(sources) $(operators) $(outputs) $(io) \
	$(analyze) $(playlists) \
	lang/lang_builtins.ml
libliq_objects=$(libliq_sources:.ml=.$(o))
liquidsoap.$(a): $(libliq_objects:.c=.o) Makefile
	ocamlmklib $(OCAML_CFLAGS:-threads:) -linkall $(OCAML_LFLAGS) \
	  -o liquidsoap \
	  $(libliq_objects:.c=.o)
