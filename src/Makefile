SUBDIRS=tools \
	$(if $(WIN32_HOST),win32) \
	$(if $(UNIX_HOST),unix) \
	decoder playlists encoder protocols plugins stream lang \
	ogg_formats converters operators sources conversions outputs io \
	visualization analyze synth harbor lang_encoders encoder_formats

DISTFILES = $(wildcard *.mli) Makefile $(wildcard *.ml) META.in

ocaml_progs= liquidsoap

top_srcdir=..
include $(top_srcdir)/Makefile.defs

liquidsoap_c_files = tools/unix_c.c \
  $(if $(W_OSS),io/oss_io_c.c)

ogg_demuxer = \
  $(if $(W_SPEEX),ogg_formats/speex_decoder.ml) \
  $(if $(W_VORBIS),ogg_formats/vorbis_decoder.ml) \
  $(if $(W_OPUS),ogg_formats/opus_decoder.ml) \
  $(if $(W_THEORA),ogg_formats/theora_decoder.ml) \
  $(if $(W_FLAC_OGG),ogg_formats/ogg_flac_decoder.ml)

ogg_muxer = \
  $(if $(W_OGG),ogg_formats/ogg_muxer.ml encoder/ogg_encoder.ml) \
  $(if $(W_SPEEX),ogg_formats/speex_encoder.ml) \
  $(if $(W_VORBIS),ogg_formats/vorbis_encoder.ml) \
  $(if $(W_OPUS),ogg_formats/opus_encoder.ml) \
  $(if $(W_THEORA),ogg_formats/theora_encoder.ml) \
  $(if $(W_FLAC_OGG),ogg_formats/ogg_flac_encoder.ml)

audio_converters = \
	converters/audio/native_audio_converter.ml \
	$(if $(W_SAMPLERATE),converters/audio/libsamplerate_converter.ml)

video_converters = \
	converters/video_converter.ml \
	converters/video/native_video_converter.ml \
	$(if $(W_GAVL),converters/video/gavl_converter.ml) \
	$(if $(W_FFMPEG),converters/video/ffmpeg_video_converter.ml)

decoders = \
	decoder/metadata_decoder.ml \
	decoder/wav_aiff_decoder.ml decoder/midi_decoder.ml \
	decoder/image_decoder.ml decoder/image/ppm_decoder.ml \
	decoder/external_decoder.ml decoder/raw_audio_decoder.ml \
	$(if $(W_FFMPEG),decoder/ffmpeg_decoder.ml) \
	$(if $(W_FLAC),decoder/flac_decoder.ml) \
	$(if $(W_FAAD),decoder/aac_decoder.ml) \
	$(if $(W_OGG),decoder/ogg_decoder.ml) \
	$(if $(W_MAD),decoder/mad_decoder.ml) \
	$(if $(W_VORBIS),decoder/vorbisduration.ml) \
	$(if $(W_FLAC),decoder/flacduration.ml) \
	$(if $(W_FLAC_OGG),decoder/ogg_flac_duration.ml) \
	$(if $(W_TAGLIB),decoder/taglib_plug.ml) \
	$(if $(W_CAMLIMAGES),decoder/image/camlimages_decoder.ml) \
	$(if $(W_SDL),decoder/image/sdlimage_decoder.ml) \
	$(if $(W_GSTREAMER),decoder/gstreamer_decoder.ml) \

playlists = \
	playlists/playlist_basic.ml \
	$(if $(W_XMLPLAYLIST),playlists/playlist_xml.ml)

protocols = \
	protocols/annotate.ml \
	protocols/mpd.ml

sources = \
	sources/synthesized.ml sources/noise.ml sources/blank.ml \
	sources/audio_gen.ml sources/request_source.ml sources/latest_metadata.ml \
	sources/req_simple.ml sources/playlist.ml \
	sources/req_queue.ml sources/req_equeue.ml \
	sources/generated.ml sources/http_source.ml \
	$(if $(W_SSL),sources/https_source.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),sources/https_secure_transport_source.ml) \
	harbor/harbor_base.ml harbor/harbor.ml sources/harbor_input.ml \
	$(if $(W_SSL),harbor/harbor_ssl.ml sources/harbor_input_ssl.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),harbor/harbor_secure_transport.ml sources/harbor_input_secure_transport.ml) \
	sources/external_input.ml \
	tools/liqMM.ml \
	$(if $(W_BJACK),sources/bjack_in.ml) \
	$(if $(W_ALSA),sources/alsa_in.ml)

operators = \
	operators/insert_metadata.ml operators/map_metadata.ml \
	operators/on_metadata.ml operators/store_metadata.ml \
	operators/on_track.ml operators/on_end.ml operators/delay.ml \
	operators/max_duration.ml operators/sequence.ml operators/add.ml \
	operators/switch.ml operators/cross.ml operators/chop.ml \
	operators/compress_exp.ml operators/pitch.ml operators/pipe.ml \
	operators/fir_filter.ml operators/iir_filter.ml operators/filter_rc.ml \
	operators/biquad_filter.ml operators/cuepoint.ml \
	operators/map_op.ml operators/flanger.ml operators/comb.ml \
	operators/compress.ml operators/clip.ml operators/filter.ml \
	operators/amplify.ml operators/normalize.ml operators/echo.ml \
	operators/append.ml operators/pan.ml operators/ms_stereo.ml \
	operators/dyn_op.ml operators/video_effects.ml operators/video_fade.ml \
	operators/noblank.ml operators/compand.ml operators/on_offset.ml \
	operators/mixing_table.ml operators/prepend.ml \
	operators/midi_routing.ml operators/sleeper.ml \
	operators/time_warp.ml operators/resample.ml \
	operators/chord.ml operators/video_text.ml operators/window_op.ml \
	$(if $(W_SOUNDTOUCH),operators/soundtouch_op.ml) \
	$(if $(W_SOUNDTOUCH),operators/st_bpm.ml) \
	$(if $(W_LADSPA),operators/ladspa_op.ml) \
	$(if $(W_FREI0R),operators/frei0r_op.ml) \
	$(if $(W_GD),operators/video_text_gd.ml) \
	$(if $(W_SDL),operators/video_text_sdl.ml) \
	$(if $(W_GSTREAMER),operators/video_text_gstreamer.ml)

conversions = \
	conversions/audio_to_stereo.ml conversions/drop.ml conversions/id.ml \
	conversions/swap.ml conversions/mean.ml conversions/mux.ml

analyze = analyze/analyze_rms.ml

encoders = \
	encoder/wav_encoder.ml \
	encoder/avi_encoder.ml \
	encoder/lame_encoder.ml \
	encoder/fdkaac_encoder.ml \
	encoder/external_encoder.ml \
	$(if $(W_LAME_DYN),encoder/lame_encoder_dynlink.ml) \
	$(if $(W_FDKAAC_DYN),encoder/fdkaac_encoder_dynlink.ml) \
	$(if $(W_SHINE),encoder/shine_encoder.ml) \
	$(if $(W_LAME),encoder/lame_encoder_builtin.ml) \
	$(if $(W_FLAC),encoder/flac_encoder.ml) \
	$(if $(W_FDKAAC),encoder/fdkaac_encoder_builtin.ml) \
	$(if $(W_TAGLIB),encoder/taglib_id3v2.ml) \
	$(if $(W_GSTREAMER),encoder/gstreamer_encoder.ml)

lang_encoders = \
  lang/lang_encoders.ml \
  $(wildcard lang_encoders/*.ml)

encoder_formats = \
  encoder_formats.ml \
  encoder_formats/meta_format.ml \
  encoder_formats/avi_format.ml \
  encoder_formats/external_encoder_format.ml \
  encoder_formats/fdkaac_format.ml \
  encoder_formats/flac_format.ml \
  encoder_formats/gstreamer_format.ml \
  encoder_formats/mp3_format.ml \
  encoder_formats/opus_format.ml \
  encoder_formats/shine_format.ml \
  encoder_formats/speex_format.ml \
  encoder_formats/theora_format.ml \
  encoder_formats/vorbis_format.ml \
  encoder_formats/ogg_format.ml \
  encoder_formats/wav_format.ml

outputs = outputs/output.ml \
	outputs/pipe_output.ml outputs/hls_output.ml \
	outputs/harbor_output.ml \
	$(if $(W_SSL),outputs/harbor_output_ssl.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),outputs/harbor_output_secure_transport.ml) \
	$(if $(W_AO),outputs/ao_out.ml) \
	$(if $(W_CRY),outputs/icecast2.ml) \
	$(if $(W_GRAPHICS),outputs/graphics_out.ml) \
	$(if $(W_BJACK),outputs/bjack_out.ml) \
	$(if $(W_SDL),outputs/sdl_out.ml) \
	$(if $(W_ALSA),outputs/alsa_out.ml)

io = io/udp_io.ml \
	$(if $(W_OSS),io/oss_io.ml) \
	$(if $(W_PULSEAUDIO),io/pulseaudio_io.ml) \
	$(if $(W_PORTAUDIO),io/portaudio_io.ml) \
	$(if $(W_ALSA),io/alsa_io.ml) \
	$(if $(W_GSTREAMER),io/gstreamer_io.ml)

tools = tools/extralib.ml tools/JSON.ml tools/doc.ml \
        tools/utils.ml tools/plug.ml \
	$(if $(WIN32_HOST),win32/process_utils.ml) \
	$(if $(UNIX_HOST),unix/process_utils.ml) \
	tools/rqueue.ml tools/sedlexing_compat.ml \
	$(if $(W_LAME_DYN),tools/lame_dynlink.ml) \
	$(if $(W_FDKAAC_DYN),tools/fdkaac_dynlink.ml) \
	tools/wav_aiff.ml tools/tutils.ml \
	tools/file_watcher.ml tools/file_watcher_mtime.ml \
	$(if $(W_DYNLINK),tools/dyntools.ml) \
	configure.ml tools/console.ml tools/process_handler.ml \
	$(if $(W_FFMPEG),tools/ffmpeg_config.ml) \
	tools/http.ml \
	$(if $(W_SSL),tools/https.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),tools/https_secure_transport.ml) \
	tools/pool.ml tools/sha1.ml tools/websocket.ml \
	$(if $(W_INOTIFY),tools/file_watcher_inotify.ml)

stream =stream/frame.ml stream/generator.ml \
	stream/aFrame.ml stream/vFrame.ml stream/mFrame.ml \
	$(if $(W_GSTREAMER),tools/gstreamer_utils.ml)

visualization = visualization/vumeter.ml \
	visualization/midimeter.ml \
	visualization/video_volume.ml \
	$(if $(W_GRAPHICS),visualization/vis_volume.ml)

synth = synth/keyboard.ml synth/synth_op.ml \
	$(if $(W_DSSI),synth/dssi_op.ml) \
	$(if $(W_SDL),synth/keyboard_sdl.ml)

builtins = lang/lang_builtins.ml lang/builtins_json.ml \
           lang/builtins_harbor.ml lang/builtins_dtools.ml \
           lang/builtins_time.ml lang/builtins_callbacks.ml \
           lang/builtins_server.ml lang/builtins_math.ml \
	   lang/builtins_files.ml lang/builtins_resolvers.ml \
           $(if $(W_SSL),lang/builtins_https.ml lang/builtins_harbor_ssl.ml) \
           $(if $(W_OSX_SECURE_TRANSPORT),lang/builtins_https_secure_transport.ml lang/builtins_harbor_secure_transport.ml) \
           $(if $(W_LO),lang/builtins_lo.ml) \
           $(if $(W_MAGIC),lang/builtins_magic.ml) \
           $(if $(W_CRY),lang/builtins_cry.ml) \
           $(if $(W_LASTFM),lang/builtins_lastfm.ml)

liquidsoap_sources= \
	tools/log.ml $(tools) REVISION.ml converters/audio_converter.ml $(stream) \
	decoder/decoder.ml request.ml tools/server.ml \
        source.ml clock.ml tools/rutils.ml \
	playlist_parser.ml $(encoder_formats) encoder.ml \
	tools/server_builtins.ml \
	$(if $(W_SDL),tools/sdl_utils.ml) \
	$(if $(W_LASTFM),tools/liqfm.ml) \
	$(if $(W_ALSA), alsa_settings.ml)

liquidsoap_sources += \
	lang/lang_types.ml lang/lang_values.ml \
	$(lang_encoders) lang/lang_parser.ml lang/lang_lexer.ml \
	lang/lang_pp.ml lang/lang_errors.ml lang/lang.ml \
        tools/start_stop.ml tools/ioRing.ml \
	tools/icecast_utils.ml tools/avi.ml \
	$(video_converters) $(audio_converters) $(ogg_demuxer) $(protocols) \
	$(sources) $(conversions) $(outputs) $(operators) \
	$(encoders) $(ogg_muxer) $(io) \
	$(analyze) $(playlists) $(visualization) $(synth) $(decoders) \
	shebang.ml $(builtins) main.ml \
	$(if $(W_WINSVC),runner_service.ml,runner.ml)

include $(top_srcdir)/Makefile.rules

configure.ml:
	@echo
	@echo " ERROR Missing src/configure.ml, Please run ./configure first!"
	@if [ -f ../configure-with-options ] ; then \
	  echo "       Note that you can source configure-with-options" ; \
	  echo "       for running configure with the same options as last time." \
	; fi
	@echo
	@exit 1

OCAML_CFLAGS= -thread $(patsubst %,-I %,$(SUBDIRS))
DEP_OPTS= $(patsubst %,-I %,$(SUBDIRS))

# Compute something like " (trunk/liquidsoap@rev1234)" when .svn is present,
REVISION.ml: Makefile $(liquidsoap_sources:REVISION.ml=)
	if test -e ../.git && which git > /dev/null ; then \
	  url=`git remote -v | grep origin | grep fetch | sed 's/origin//' | sed 's/ (.*)//' | tr -d '\t'` ; \
	  head=`git rev-parse HEAD` ; \
          date=`date +%Y%m%d:%H%M%S`; \
	  echo "let rev = \" ($$url@$$head:$$date)\"" > REVISION.ml ; \
	elif test -d .svn && which svn > /dev/null ; then \
	  url=`LC_ALL=C svn info | grep URL | \
	       sed -e 's:.*svnroot/savonet/\(.*\)/src:\1:'`; \
	  revision=`LC_ALL=C svn info | grep Revision | \
	            sed -e 's/Revision: \(.*\)/\1/'`  ; \
	  echo "let rev = \" ($$url@rev$$revision)\"" > REVISION.ml ; \
	elif test -d ../../.hg && which hg > /dev/null ; then \
	  branch=`hg branch`; \
	  revision=`LC_ALL=C hg parents | grep changeset: | \
	       sed -e 's/changeset: *['0'-'9']*:\([^ ]*\).*/\1/'`; \
          date=`date +%Y%m%d:%H%M%S`; \
	  echo "let rev = \" ($$branch@$$revision:$$date)\"" > REVISION.ml ; \
	else \
	  echo "let rev = \"\"" > REVISION.ml ; \
	fi

clean-local:
	rm -f REVISION.ml

install-local: all-auto
	$(INSTALL) -d $(bindir)
	$(INSTALL_PROGRAM) liquidsoap $(bindir)
ifneq ($(BYTE),)
  ifneq ($(NO_CUSTOM),)
	$(OCAMLFIND) install liquidsoap META dllliquidsoap.so $(wildcard plugins/dll*.so)
  else
	$(OCAMLFIND) install liquidsoap META $(wildcard plugins/dll*.so)
  endif
endif

# Dirty hack since ocamlcp does not play nicely with include
profile:
	-$(MAKE)
	cd lang; ocamlc -c -I .. -I ../tools lang_values.ml
	$(MAKE)
