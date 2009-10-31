
SUBDIRS= operators sources tools formats protocols outputs io plugins stream \
         visualization lang analyze playlists ogg_formats video_converters \
         audio_converters synth encoder

DISTFILES = $(wildcard *.mli) Makefile $(wildcard *.ml) META.in

ocaml_progs= liquidsoap

ogg_utils =
# \
	$(if $(W_OGG),ogg_demuxer.ml) \
	$(if $(W_OGG),ogg_encoder.ml) \
	$(if $(W_SPEEX),ogg_formats/speex_format.ml) \
	$(if $(W_THEORA),ogg_formats/theora_format.ml) \
	$(if $(W_VORBIS),ogg_formats/vorbis_format.ml) \
	$(if $(W_SCHROEDINGER),ogg_formats/dirac_format.ml)

audio_converters = \
	audio_converters/native_audio_converter.ml \
	$(if $(W_SAMPLERATE),audio_converters/libsamplerate_converter.ml)

video_converters = \
	video_converter.ml \
	video_converters/native_video_converter.ml \
	$(if $(W_GAVL),video_converters/gavl_converter.ml)

formats = \
	$(if $(W_MP3),formats/mp3.ml) \
	# $(if $(W_OGG),formats/oggformat.ml) \
	$(if $(W_VORBIS),formats/vorbisduration.ml) \
	$(if $(W_TAGLIB),formats/taglib_plug.ml) \
	$(if $(W_FAAD),formats/aacformat.ml) \
	formats/wavformat.ml \
	formats/externalformat.ml \
	formats/midiformat.ml

playlists = \
	playlists/playlist_basic.ml $(if $(W_XMLPLAYLIST),playlists/playlist_xml.ml)

protocols = \
	protocols/say.ml protocols/annotate.ml \
	protocols/mpd.ml protocols/extproto.ml \
	$(if $(W_LAST),protocols/lastfm_req.ml)

sources = sources/blank.ml sources/sine.ml sources/square.ml sources/saw.ml \
	sources/request_source.ml \
	sources/req_simple.ml sources/playlist.ml \
	sources/req_queue.ml sources/req_equeue.ml \
	sources/generated.ml sources/http_source.ml
	# sources/external_input.ml \
	$(if $(W_OGG),sources/http_ogg.ml) \
	sources/noise.ml sources/harbor_input.ml \
	$(if $(W_MP3),sources/http_mp3.ml) \
	$(if $(W_FAAD),sources/http_aac.ml) \
	$(if $(W_ALSA),sources/alsa_in.ml) \
	$(if $(W_LAST),sources/lastfm_input.ml) \
	$(if $(W_BJACK),sources/bjack_in.ml) \
	sources/video_noise.ml \
	sources/video_image.ml \
	$(if $(W_SDL),sources/video_text.ml)

operators = \
	operators/insert_metadata.ml operators/map_metadata.ml \
	operators/on_metadata.ml operators/store_metadata.ml \
	operators/on_track.ml operators/on_end.ml operators/delay.ml \
	operators/switch.ml operators/id.ml operators/fade.ml # operators/add.ml \
	operators/setvol.ml operators/normalize.ml operators/echo.ml \
	operators/prepend.ml operators/append.ml operators/accel.ml \
	operators/noblank.ml operators/compand.ml \
	operators/sequence.ml operators/cross.ml operators/smartcross.ml \
	operators/mixing_table.ml operators/filter.ml \
	operators/mean.ml operators/swap.ml \
	operators/pan.ml operators/ms_stereo.ml \
	operators/compress_exp.ml operators/pitch.ml \
	operators/fir_filter.ml operators/iir_filter.ml \
	operators/biquad_filter.ml operators/substract.ml \
	operators/map_op.ml operators/resample.ml \
	operators/flanger.ml operators/comb.ml \
	operators/compress.ml operators/clip.ml \
	$(if $(W_SOUNDTOUCH),operators/soundtouch_op.ml) \
	$(if $(W_SOUNDTOUCH),operators/st_bpm.ml) \
	$(if $(W_LADSPA),operators/ladspa_op.ml) \
	operators/time_wrap.ml operators/lag.ml \
	operators/video_effects.ml operators/video_fade.ml \
	operators/fork.ml operators/midi_routing.ml
#	operators/pipe.ml

analyze =
# analyze/analyze_rms.ml

encoders = encoder/wav_encoder.ml
	# $(if $(W_OGG),outputs/ogg_output.ml) \
	$(if $(W_LAME),outputs/lame_encoded.ml) \
	$(if $(W_AACPLUS),outputs/aacplus_encoded.ml) \
	$(if $(W_VORBIS), outputs/vorbis_encoded.ml)  \
	$(if $(W_THEORA),outputs/theora_encoded.ml) \
	$(if $(W_SCHROEDINGER),outputs/dirac_encoded.ml) \
	$(if $(W_SPEEX), outputs/speex_encoded.ml) \
	$(if $(W_FAAC),outputs/faac_encoded.ml)

outputs = outputs/output.ml $(if $(W_AO),outputs/ao_out.ml) \
	outputs/file_output.ml $(encoders) \
	# outputs/external_encoded.ml \
	# $(if $(W_CRY),outputs/icecast2.ml \
	$(if $(W_ALSA),outputs/alsa_out.ml) \
	$(if $(W_BJACK),outputs/bjack_out.ml) \
	$(if $(W_SDL),outputs/sdl_out.ml) \
	$(if $(W_GRAPHICS),outputs/graphics_out.ml)

io =
# $(if $(W_OSS),io/oss_io_c.c io/oss_io.ml) \
	$(if $(W_ALSA),io/alsa_io.ml) \
	$(if $(W_PORTAUDIO),io/portaudio_io.ml) \
	$(if $(W_PULSEAUDIO),io/pulseaudio_io.ml) \
	$(if $(W_JACK),io/jack_io.ml) \
	$(if $(W_V4L),io/v4l_io_c.c) \
	$(if $(W_V4L),io/v4l_io.ml) \
	$(if $(W_OPAL),io/opal_io_c.o io/opal_io.ml) \
	io/marshal_io.ml

tools = tools/doc.ml tools/plug.ml tools/utils.ml \
	tools/rqueue.ml tools/ringbuffer.ml \
	$(if $(NEED_TCP_NODELAY), tools/socket_stubs.c) \
	tools/liq_sockets.ml \
	tools/wav.ml $(if $(BYTE),tools/dynliq.ml) configure.ml \
	tools/tutils.ml tools/sutils.ml tools/http.ml

stream = stream/midi.ml stream/rgb_c.c stream/RGB.ml \
	stream/frame.ml stream/generator.ml decoder.ml playlist_parser.ml \
	request.ml source.ml root.ml \
	stream/float_pcm_c.c stream/float_pcm.ml \
	stream/aFrame.ml stream/vFrame.ml stream/mFrame.ml

visualization =
# visualization/vumeter.ml \
	$(if $(W_GRAPHICS),visualization/vis_volume.ml) \
	$(if $(W_GL),visualization/GLvisualizer.ml) \
	$(if $(W_GL),visualization/vis_glvolume.ml) \
	visualization/video_volume.ml \
        visualization/midimeter.ml

synth =
# synth/synth.ml synth/synth_op.ml \
	synth/keyboard.ml \
	$(if $(W_SDL),synth/keyboard_sdl.ml)

liquidsoap_sources= $(tools) SVN.ml audio_converter.ml $(stream) \
	encoder/encoder.ml \
	tools/server.ml tools/server_builtins.ml \
	$(if $(W_LAST),tools/liqfm.ml) \
	$(if $(W_ALSA), alsa_settings.ml) \
	lang/lang_types.ml lang/lang_values.ml \
	lang/lang_lexer.ml lang/lang_parser.ml lang/lang_pp.ml lang/lang.ml \
	tools/ioRing.ml tools/file_decoder.ml tools/mutils.ml \
	$(video_converters) $(audio_converters) \
	$(ogg_utils) $(protocols) $(sources) $(operators) $(outputs) $(io) \
	$(analyze) $(playlists) $(visualization) $(synth) $(formats) \
	shebang.ml \
	lang/lang_builtins.ml \
	$(if $(W_MAGIC),lang/lang_magic.ml) \
	$(if $(W_LAST),lang/lang_lastfm.ml) \
	main.ml
# tools/harbor.ml 

OCAML_CFLAGS= -thread \
	-I tools -I lang -I playlists -I stream -I outputs -I formats \
	-I video_converters -I sources -I operators -I io -I visualization \
	-I analyze -I ogg_formats -I audio_converters -I synth -I encoder
OCAML_LFLAGS= $(if $(BYTE),dynlink.cma) $(if $(W_OPAL),-cclib -lopal)
DEP_OPTS= -I sources -I operators -I tools -I lang -I formats -I playlists \
		  -I video_converters -I outputs -I io -I stream -I visualization \
		  -I analyze -I ogg_formats -I audio_converters -I synth -I encoder

liquidsoap_doc_sources= \
	source.ml root.ml \
	tools/plug.ml tools/http.ml \
	decoder.ml request.ml outputs/output.ml \
	lang/lang.ml tools/server.ml \
	tools/tutils.ml tools/sutils.ml tools/mutils.ml \
	tools/ringbuffer.ml \
	tools/doc.ml tools/wav.ml \
	sources/request_source.ml operators/switch.ml \
	configure.ml video_converter.ml \
	$(if $(W_OGG),ogg_encoder.ml ogg_demuxer.ml) \
	stream/frame.ml stream/float_pcm.ml stream/aFrame.ml \
	stream/vFrame.ml stream/RGB.ml audio_converter.ml \
	stream/mFrame.ml

top_srcdir=..
include $(top_srcdir)/Makefile.rules

stream/float_pcm_c.o: stream/float_pcm_c.c
	$(MAKE) -C stream float_pcm_c.o
stream/rgb_c.o: stream/rgb_c.c
	$(MAKE) -C stream rgb_c.o
io/oss_io_c.o: io/oss_io_c.c
	$(MAKE) -C io oss_io_c.o
io/opal_io_c.o: io/opal_io_c.c
	$(MAKE) -C io opal_io_c.o

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

install-local: all-auto
	$(INSTALL) -d $(bindir)
	$(INSTALL_PROGRAM) liquidsoap $(bindir)

# Dirty hack since ocamlcp does not play nicely with include
profile:
	-$(MAKE)
	cd lang; ocamlc -c -I .. -I ../tools lang_values.ml
	$(MAKE)

libliq_sources= $(tools) SVN.ml audio_converter.ml $(stream) \
	shebang.ml alsa_settings.ml \
	tools/server.ml tools/server_builtins.ml \
	lang/lang_types.ml lang/lang_values.ml \
	lang/lang_lexer.ml lang/lang_parser.ml lang/lang_pp.ml lang/lang.ml \
	tools/harbor.ml $(video_converters) $(audio_converters) \
	$(ogg_utils) $(protocols) $(sources) $(operators) $(outputs) $(io) \
	$(analyze) $(playlists) $(formats) \
	lang/lang_builtins.ml
libliq_objects=$(libliq_sources:.ml=.$(o))
liquidsoap.$(a): $(libliq_objects:.c=.o) Makefile
	ocamlmklib $(OCAML_CFLAGS:-threads:) -linkall $(OCAML_LFLAGS) \
	  -o liquidsoap \
	  $(libliq_objects:.c=.o)
