SUBDIRS=tools decoder playlists encoder protocols plugins stream lang \
	ogg_formats converters operators sources conversions outputs io \
	visualization analyze synth

DISTFILES = $(wildcard *.mli) Makefile $(wildcard *.ml) META.in

ocaml_progs= liquidsoap

top_srcdir=..
include $(top_srcdir)/Makefile.defs

# Liquidsoap source files work as follows.
# Each file has to be either added to liquidsoap's files if compiled builtin
# or to the corresponding plugin if built as a plugin.
# For this purpose, we define the make function
# conditional_compilation, which redirects each of those files to
# the correct make variable. Its parameters are:
# $1: name of the make variable where the file should go if compiled builtin.
# $2: the $W_FOO variable
# $3: path to the file
# $4: type of source: empty for .ml sources and c_ for c sources.
#
# For now, the old style Makefile has been converted to
# this new style by replacing $(if $(W_FOO),..) by
# $(call conditional_compilation,..). However, this may
# cause some trouble as the order of declaration is important
# both for the builtin case and plugin case.. See for instance
# SDL files, whose declaration have been moved at the end.
conditional_compilation = \
  $(if $(2),\
    $(if $(2:yes=),\
      $(eval $(2)_$(4)sources += $(3)),\
      $(eval $(1) += $(3))))

liquidsoap_c_files = tools/locale_c.c

# Add conditional files..
$(call conditional_compilation,liquidsoap_c_files,$(W_OSS),io/oss_io_c.c,c_)

# This variable contains only conditional files..
ogg_demuxer =

$(call conditional_compilation,ogg_demuxer,$(W_SPEEX),ogg_formats/speex_decoder.ml)
$(call conditional_compilation,ogg_demuxer,$(W_VORBIS),ogg_formats/vorbis_decoder.ml)
$(call conditional_compilation,ogg_demuxer,$(W_OPUS),ogg_formats/opus_decoder.ml)
$(call conditional_compilation,ogg_demuxer,$(W_THEORA),ogg_formats/theora_decoder.ml)
$(call conditional_compilation,ogg_demuxer,$(W_FLAC_OGG),ogg_formats/ogg_flac_decoder.ml)
$(call conditional_compilation,ogg_demuxer,$(W_SCHROEDINGER),ogg_formats/dirac_decoder.ml)

# This variable contains only conditional files..
ogg_muxer =

$(call conditional_compilation,ogg_muxer,$(W_OGG),ogg_formats/ogg_muxer.ml)
$(call conditional_compilation,ogg_muxer,$(W_OGG), encoder/ogg_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_SPEEX),ogg_formats/speex_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_VORBIS),ogg_formats/vorbis_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_OPUS),ogg_formats/opus_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_THEORA),ogg_formats/theora_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_FLAC_OGG),ogg_formats/ogg_flac_encoder.ml)
$(call conditional_compilation,ogg_muxer,$(W_SCHROEDINGER),ogg_formats/dirac_encoder.ml)

audio_converters = \
	converters/audio/native_audio_converter.ml

$(call conditional_compilation,audio_converters,$(W_SAMPLERATE),converters/audio/libsamplerate_converter.ml)

video_converters = \
	converters/video_converter.ml \
	converters/video/native_video_converter.ml

$(call conditional_compilation,video_converters,$(W_GAVL),converters/video/gavl_converter.ml)
$(call conditional_compilation,video_converters,$(W_FFMPEG),converters/video/ffmpeg_video_converter.ml)

decoders = \
	decoder/metadata_decoder.ml \
	decoder/wav_aiff_decoder.ml decoder/midi_decoder.ml \
	decoder/image_decoder.ml decoder/image/ppm_decoder.ml \
	decoder/external_decoder.ml decoder/raw_audio_decoder.ml

$(call conditional_compilation,decoders,$(W_FLAC),decoder/flac_decoder.ml)
$(call conditional_compilation,decoders,$(W_FAAD),decoder/aac_decoder.ml)
$(call conditional_compilation,decoders,$(W_OGG),decoder/ogg_decoder.ml)
$(call conditional_compilation,decoders,$(W_MAD),decoder/mad_decoder.ml)
$(call conditional_compilation,decoders,$(W_VORBIS),decoder/vorbisduration.ml)
$(call conditional_compilation,decoders,$(W_FLAC),decoder/flacduration.ml)
$(call conditional_compilation,decoders,$(W_FLAC_OGG),decoder/ogg_flac_duration.ml)
$(call conditional_compilation,decoders,$(W_TAGLIB),decoder/taglib_plug.ml)

playlists = \
	playlists/playlist_basic.ml

$(call conditional_compilation,playlists,$(W_XMLPLAYLIST),playlists/playlist_xml.ml)

protocols = \
	protocols/annotate.ml \
	protocols/mpd.ml \
	protocols/extproto.ml

ifneq ($(OS_TYPE), "Win32")
protocols += protocols/say.ml
endif

sources = \
	sources/synthesized.ml sources/noise.ml sources/blank.ml \
	sources/audio_gen.ml sources/request_source.ml \
	sources/req_simple.ml sources/playlist.ml \
	sources/req_queue.ml sources/req_equeue.ml \
	sources/generated.ml sources/http_source.ml \
	tools/harbor.ml sources/harbor_input.ml \
	sources/external_input.ml \
	tools/liqMM.ml

$(call conditional_compilation,sources,$(W_BJACK),sources/bjack_in.ml)

operators = \
	operators/insert_metadata.ml operators/map_metadata.ml \
	operators/on_metadata.ml operators/store_metadata.ml \
	operators/on_track.ml operators/on_end.ml operators/delay.ml \
	operators/switch.ml operators/fade.ml operators/add.ml \
	operators/sequence.ml operators/cross.ml operators/smartcross.ml \
	operators/compress_exp.ml operators/pitch.ml \
	operators/fir_filter.ml operators/iir_filter.ml operators/filter_rc.ml \
	operators/biquad_filter.ml operators/cuepoint.ml \
	operators/map_op.ml operators/flanger.ml operators/comb.ml \
	operators/compress.ml operators/clip.ml operators/filter.ml \
	operators/setvol.ml operators/normalize.ml operators/echo.ml \
	operators/append.ml operators/pan.ml operators/ms_stereo.ml \
	operators/dyn_op.ml operators/video_effects.ml operators/video_fade.ml \
	operators/noblank.ml operators/compand.ml \
	operators/mixing_table.ml operators/prepend.ml \
	operators/midi_routing.ml operators/sleeper.ml \
	operators/time_warp.ml operators/resample.ml \
	operators/chord.ml operators/video_text.ml operators/rms_op.ml

$(call conditional_compilation,operators,$(W_SOUNDTOUCH),operators/soundtouch_op.ml)
$(call conditional_compilation,operators,$(W_SOUNDTOUCH),operators/st_bpm.ml)
$(call conditional_compilation,operators,$(W_LADSPA),operators/ladspa_op.ml)
$(call conditional_compilation,operators,$(W_FREI0R),operators/frei0r_op.ml)

conversions = \
	conversions/audio_to_stereo.ml conversions/drop.ml conversions/id.ml \
	conversions/swap.ml conversions/mean.ml conversions/mux.ml

analyze = analyze/analyze_rms.ml

encoders = \
	encoder/wav_encoder.ml \
	encoder/avi_encoder.ml \
	encoder/lame_encoder.ml \
	encoder/fdkaac_encoder.ml \
	encoder/aacplus_encoder.ml \
	encoder/external_encoder.ml \
	$(if $(W_LAME_DYN),encoder/lame_encoder_dynlink.ml) \
	$(if $(W_FDKAAC_DYN),encoder/fdkaac_encoder_dynlink.ml) \
	$(if $(W_AACPLUS_DYN),encoder/aacplus_encoder_dynlink.ml)

$(call conditional_compilation,encoders,$(W_SHINE),encoder/shine_encoder.ml)
$(call conditional_compilation,encoders,$(W_LAME),encoder/lame_encoder_builtin.ml)
$(call conditional_compilation,encoders,$(W_FLAC),encoder/flac_encoder.ml)
$(call conditional_compilation,encoders,$(W_AACPLUS),encoder/aacplus_encoder_builtin.ml)
$(call conditional_compilation,encoders,$(W_VOAACENC),encoder/voaacenc_encoder.ml)
$(call conditional_compilation,encoders,$(W_FDKAAC),encoder/fdkaac_encoder_builtin.ml)
$(call conditional_compilation,encoders,$(W_TAGLIB),encoder/taglib_id3v2.ml)

outputs = outputs/output.ml \
	outputs/pipe_output.ml \
	outputs/harbor_output.ml

$(call conditional_compilation,outputs,$(W_AO),outputs/ao_out.ml)
$(call conditional_compilation,outputs,$(W_CRY),outputs/icecast2.ml)
$(call conditional_compilation,outputs,$(W_GRAPHICS),outputs/graphics_out.ml)
$(call conditional_compilation,outputs,$(W_BJACK),outputs/bjack_out.ml)

io = io/udp_io.ml

$(call conditional_compilation,io,$(W_OSS),io/oss_io.ml)
$(call conditional_compilation,io,$(W_PULSEAUDIO),io/pulseaudio_io.ml)
$(call conditional_compilation,io,$(W_PORTAUDIO),io/portaudio_io.ml)

tools = tools/stdlib.ml tools/doc.ml tools/utils.ml tools/plug.ml \
	$(if $(W_DYNLINK),tools/dyntools.ml) \
	tools/rqueue.ml \
	$(if $(W_LAME_DYN),tools/lame_dynlink.ml) \
	$(if $(W_FDKAAC_DYN),tools/fdkaac_dynlink.ml) \
	$(if $(W_AACPLUS_DYN),tools/aacplus_dynlink.ml) \
	tools/wav_aiff.ml tools/tutils.ml tools/JSON.ml \
	tools/file_watcher.ml tools/file_watcher_mtime.ml \
	configure.ml \
	tools/http.ml tools/pool.ml \
	tools/sha1.ml tools/websocket.ml

$(call conditional_compilation,tools,$(W_INOTIFY),tools/file_watcher_inotify.ml)

stream =stream/frame.ml stream/generator.ml \
	stream/aFrame.ml stream/vFrame.ml stream/mFrame.ml

visualization = visualization/vumeter.ml \
	visualization/midimeter.ml \
	visualization/video_volume.ml

$(call conditional_compilation,visualization,$(W_GRAPHICS),visualization/vis_volume.ml)

synth = synth/keyboard.ml synth/synth_op.ml

$(call conditional_compilation,synth,$(W_DSSI),synth/dssi_op.ml)

builtins = lang/lang_builtins.ml lang/builtins_json.ml

$(call conditional_compilation,builtins,$(W_LO),lang/builtins_lo.ml)
$(call conditional_compilation,builtins,$(W_MAGIC),lang/builtins_magic.ml)
$(call conditional_compilation,builtins,$(W_CRY),lang/builtins_cry.ml)

liquidsoap_sources= shutdown.ml \
	$(tools) SVN.ml converters/audio_converter.ml $(stream) \
	decoder/decoder.ml request.ml tools/server.ml \
        source.ml clock.ml tools/rutils.ml \
	playlist_parser.ml encoder/encoder.ml \
	tools/server_builtins.ml

# Some for camlimages
$(call conditional_compilation,decoders,$(W_CAMLIMAGES),decoder/image/camlimages_decoder.ml)

# Include SDL now. Order matters so we group them all here..
$(call conditional_compilation,liquidsoap_sources,$(W_SDL),tools/sdl_utils.ml)
$(call conditional_compilation,synth,$(W_SDL),synth/keyboard_sdl.ml)
$(call conditional_compilation,decoders,$(W_SDL),decoder/image/sdlimage_decoder.ml)
$(call conditional_compilation,operators,$(W_SDL),operators/video_text_sdl.ml)
$(call conditional_compilation,outputs,$(W_SDL),outputs/sdl_out.ml)

# Same for lastfm..
$(call conditional_compilation,liquidsoap_sources,$(W_LASTFM),tools/liqfm.ml)
$(call conditional_compilation,builtins,$(W_LASTFM),lang/builtins_lastfm.ml)

# Same for alsa..
$(call conditional_compilation,liquidsoap_sources,$(W_ALSA), alsa_settings.ml)
$(call conditional_compilation,sources,$(W_ALSA),sources/alsa_in.ml)
$(call conditional_compilation,outputs,$(W_ALSA),outputs/alsa_out.ml)
$(call conditional_compilation,io,$(W_ALSA),io/alsa_io.ml)

# Same for GD..
$(call conditional_compilation,operators,$(W_GD),operators/video_text_gd.ml)

# GStreamer
$(call conditional_compilation,stream,$(W_GSTREAMER),tools/gstreamer_utils.ml)
$(call conditional_compilation,decoders,$(W_GSTREAMER),decoder/gstreamer_decoder.ml)
$(call conditional_compilation,encoders,$(W_GSTREAMER),encoder/gstreamer_encoder.ml)
$(call conditional_compilation,operators,$(W_GSTREAMER),operators/video_text_gstreamer.ml)
$(call conditional_compilation,io,$(W_GSTREAMER),io/gstreamer_io.ml)

liquidsoap_sources += \
	lang/lang_types.ml lang/lang_values.ml \
	lang/lang_encoders.ml lang/lang_parser.ml lang/lang_lexer.ml \
	lang/lang_pp.ml lang/lang.ml tools/start_stop.ml tools/ioRing.ml \
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

liquidsoap_doc_sources= \
	source.ml clock.ml encoder/encoder.ml \
	tools/plug.ml tools/http.ml \
	decoder/decoder.ml request.ml outputs/output.ml \
	lang/lang.ml lang/lang_types.ml lang/lang_values.ml \
	tools/server.ml tools/tutils.ml playlist_parser.ml \
	tools/doc.ml tools/ioRing.ml \
	tools/dyntools.ml tools/JSON.ml \
	converters/audio_converter.ml converters/video_converter.ml \
	ogg_formats/ogg_muxer.ml \
	sources/generated.ml sources/request_source.ml sources/synthesized.ml \
	operators/switch.ml \
	configure.ml \
	stream/frame.ml stream/aFrame.ml \
	stream/vFrame.ml stream/mFrame.ml stream/generator.ml

# Compute something like " (trunk/liquidsoap@rev1234)" when .svn is present,
SVN.ml: Makefile $(liquidsoap_sources:SVN.ml=)
	if test -e ../.git && which git > /dev/null ; then \
	  url=`git remote -v | grep origin | grep fetch | sed 's/origin//' | sed 's/ (.*)//' | tr -d '\t'` ; \
	  head=`git rev-parse HEAD` ; \
          date=`date +%Y%m%d:%H%M%S`; \
	  echo "let rev = \" ($$url@$$head:$$date)\"" > SVN.ml ; \
	elif test -d .svn && which svn > /dev/null ; then \
	  url=`LC_ALL=C svn info | grep URL | \
	       sed -e 's:.*svnroot/savonet/\(.*\)/src:\1:'`; \
	  revision=`LC_ALL=C svn info | grep Revision | \
	            sed -e 's/Revision: \(.*\)/\1/'`  ; \
	  echo "let rev = \" ($$url@rev$$revision)\"" > SVN.ml ; \
	elif test -d ../../.hg && which hg > /dev/null ; then \
	  branch=`hg branch`; \
	  revision=`LC_ALL=C hg parents | grep changeset: | \
	       sed -e 's/changeset: *['0'-'9']*:\([^ ]*\).*/\1/'`; \
          date=`date +%Y%m%d:%H%M%S`; \
	  echo "let rev = \" ($$branch@$$revision:$$date)\"" > SVN.ml ; \
	else \
	  echo "let rev = \"\"" > SVN.ml ; \
	fi

clean-local:
	rm -f SVN.ml

install-plugin-%:
	$(INSTALL_DATA) plugins/$*.$(s) $(libdir)/liquidsoap/$(libs_dir_version)/plugins

install_plugins=\
$(eval INSTALL_PLUGINS+=install-plugin-$(1))
$(foreach plugin,$(liquidsoap_PLUGINS),$(call install_plugins,$(plugin)))

install-plugin-dir:
	$(INSTALL_DIRECTORY) $(libdir)/liquidsoap/$(libs_dir_version)/plugins

install-local: all-auto install-plugin-dir $(INSTALL_PLUGINS)
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
