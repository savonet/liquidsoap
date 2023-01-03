INCDIRS = builtins conversions converters decoder encoder encoder_formats harbor io lang lang_encoders ogg_formats operators outputs protocols sources stream tools $(if $(W_CAMOMILE),,tools/metadata/no-camomile) tools/metadata

liquidsoap_c_files = \
	tools/unix_c.c \
	$(if $(W_OSS),io/oss_io_c.c)

ogg_demuxer = \
	$(if $(W_SPEEX),ogg_formats/liq_speex_decoder.ml) \
	$(if $(W_VORBIS),ogg_formats/liq_vorbis_decoder.ml) \
	$(if $(W_OPUS),ogg_formats/liq_opus_decoder.ml) \
	$(if $(W_THEORA),ogg_formats/liq_theora_decoder.ml) \
	$(if $(W_FLAC_OGG),ogg_formats/liq_flac_ogg_decoder.ml)

ogg_muxer = \
	$(if $(W_OGG),ogg_formats/ogg_muxer.ml encoder/ogg_encoder.ml) \
	$(if $(W_SPEEX),ogg_formats/speex_encoder.ml) \
	$(if $(W_VORBIS),ogg_formats/vorbis_encoder.ml) \
	$(if $(W_OPUS),ogg_formats/opus_encoder.ml) \
	$(if $(W_THEORA),ogg_formats/theora_encoder.ml) \
	$(if $(W_FLAC_OGG),ogg_formats/ogg_flac_encoder.ml)

audio_converters = \
	converters/audio/native_audio_converter.ml \
	$(if $(W_SAMPLERATE),converters/audio/libsamplerate_converter.ml) \
	$(if $(W_FFMPEG),converters/audio/ffmpeg_audio_converter.ml)

video_converters = \
	converters/video/native_video_converter.ml \
	$(if $(W_FFMPEG),converters/video/ffmpeg_video_converter.ml)

text_decoders = \
	decoder/text/video_text_native.ml \
	$(if $(W_GD),decoder/text/video_text_gd.ml) \
	$(if $(W_SDL_TTF),decoder/text/video_text_sdl.ml) \
	$(if $(W_CAMLIMAGES_FREETYPE),decoder/text/video_text_camlimages.ml) \
	$(if $(W_GSTREAMER),decoder/text/video_text_gstreamer.ml)

ffmpeg_decoders = \
	decoder/ffmpeg_decoder_common.ml decoder/ffmpeg_internal_decoder.ml \
	decoder/ffmpeg_raw_decoder.ml decoder/ffmpeg_copy_decoder.ml \
	decoder/ffmpeg_decoder.ml decoder/image/ffmpeg_image_decoder.ml

decoders = \
	decoder/wav_aiff_decoder.ml decoder/midi_decoder.ml \
	decoder/image_decoder.ml decoder/image/ppm_decoder.ml \
	decoder/external_decoder.ml decoder/raw_audio_decoder.ml \
	decoder/srt_decoder.ml \
	$(text_decoders) \
	$(if $(W_FFMPEG),$(ffmpeg_decoders)) \
	$(if $(W_FLAC),decoder/liq_flac_decoder.ml) \
	$(if $(W_FAAD),decoder/aac_decoder.ml) \
	$(if $(W_OGG),decoder/liq_ogg_decoder.ml) \
	$(if $(W_MAD),decoder/mad_decoder.ml) \
	$(if $(W_VORBIS),decoder/vorbisduration.ml) \
	$(if $(W_FLAC),decoder/flacduration.ml) \
	$(if $(W_FLAC_OGG),decoder/ogg_flac_duration.ml) \
	$(if $(W_TAGLIB),decoder/taglib_plug.ml) \
        decoder/id3_plug.ml decoder/image_plug.ml decoder/video_plug.ml \
	$(if $(W_CAMLIMAGES),decoder/image/camlimages_decoder.ml) \
	$(if $(W_SDL_IMAGE),decoder/image/sdlimage_decoder.ml) \
	$(if $(W_GSTREAMER),decoder/gstreamer_decoder.ml)

playlists = \
	playlists/playlist_basic.ml \
	$(if $(W_XMLPLAYLIST),playlists/playlist_xml.ml)

protocols = \
	protocols/annotate.ml \
	protocols/mpd.ml

sources = \
	sources/synthesized.ml sources/noise.ml sources/blank.ml sources/debug_sources.ml \
	sources/audio_gen.ml sources/request_source.ml sources/latest_metadata.ml \
	sources/request_simple.ml sources/generated.ml sources/video_testsrc.ml sources/video_text.ml \
	harbor/harbor_base.ml harbor/harbor.ml sources/harbor_input.ml \
	$(if $(W_SSL),harbor/harbor_ssl.ml sources/harbor_input_ssl.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),harbor/harbor_secure_transport.ml sources/harbor_input_secure_transport.ml) \
        tools/external_input.ml sources/external_input_audio.ml sources/external_input_video.ml \
	tools/sandbox.ml \
	$(if $(W_BJACK),sources/bjack_in.ml) \
	$(if $(W_ALSA),sources/alsa_in.ml)

operators = \
	operators/insert_metadata.ml operators/map_metadata.ml \
	operators/on_end.ml operators/on_frame.ml operators/delay.ml \
	operators/on_track.ml operators/on_metadata.ml \
	operators/max_duration.ml operators/sequence.ml operators/add.ml \
	operators/switch.ml operators/cross.ml \
	operators/pitch.ml operators/pipe.ml operators/filter.ml \
	operators/fir_filter.ml operators/iir_filter.ml operators/filter_rc.ml \
	operators/biquad_filter.ml operators/cuepoint.ml \
	operators/map_op.ml operators/flanger.ml operators/comb.ml \
	operators/compress.ml operators/compress_old.ml operators/compress_exp.ml operators/clip.ml \
	operators/amplify.ml operators/normalize.ml operators/echo.ml operators/gate.ml \
	operators/append.ml operators/pan.ml operators/ms_stereo.ml \
	operators/dyn_op.ml operators/video_effects.ml operators/video_fade.ml \
	operators/noblank.ml operators/compand.ml operators/on_offset.ml \
	operators/prepend.ml operators/lufs.ml operators/available.ml \
	operators/midi_routing.ml operators/sleeper.ml \
	operators/time_warp.ml operators/resample.ml \
	operators/chord.ml operators/window_op.ml \
	operators/rms_smooth.ml operators/delay_line.ml \
	operators/accelerate.ml \
	operators/still_frame.ml operators/dtmf.ml \
	$(if $(W_SOUNDTOUCH),operators/soundtouch_op.ml) \
	$(if $(W_SOUNDTOUCH),operators/st_bpm.ml) \
	$(if $(W_LADSPA),operators/ladspa_op.ml) \
	$(if $(W_LILV),operators/lilv_op.ml) \
	$(if $(W_FREI0R),operators/frei0r_op.ml)

conversions = \
	conversions/conversion.ml conversions/audio_to_stereo.ml conversions/drop.ml \
	conversions/swap.ml conversions/mean.ml conversions/mux.ml

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
        $(if $(W_FFMPEG),encoder/ffmpeg_encoder_common.ml encoder/ffmpeg_internal_encoder.ml encoder/ffmpeg_copy_encoder.ml encoder/ffmpeg_encoder.ml) \
	$(if $(W_FDKAAC),encoder/fdkaac_encoder_builtin.ml) \
	$(if $(W_TAGLIB),encoder/taglib_id3v2.ml) \
	$(if $(W_GSTREAMER),encoder/gstreamer_encoder.ml)

lang_encoders = \
	lang_encoders/lang_avi.ml lang_encoders/lang_external_encoder.ml lang_encoders/lang_fdkaac.ml \
	lang_encoders/lang_ffmpeg.ml $(if $(W_FFMPEG),lang_encoders/lang_ffmpeg_opt.ml) \
	lang_encoders/lang_flac.ml lang_encoders/lang_gstreamer.ml \
	lang_encoders/lang_mp3.ml lang_encoders/lang_opus.ml lang_encoders/lang_shine.ml \
	lang_encoders/lang_speex.ml lang_encoders/lang_theora.ml lang_encoders/lang_vorbis.ml \
	lang_encoders/lang_wav.ml lang_encoders/lang_ogg.ml

encoder_formats = \
	encoder_formats.ml \
	encoder_formats/meta_format.ml \
	encoder_formats/avi_format.ml \
	encoder_formats/external_encoder_format.ml \
	encoder_formats/fdkaac_format.ml \
	encoder_formats/flac_format.ml \
	encoder_formats/ffmpeg_format.ml \
	encoder_formats/gstreamer_format.ml \
	encoder_formats/mp3_format.ml \
	encoder_formats/opus_format.ml \
	encoder_formats/shine_format.ml \
	encoder_formats/speex_format.ml \
	encoder_formats/theora_format.ml \
	encoder_formats/vorbis_format.ml \
	encoder_formats/ogg_format.ml \
	encoder_formats/wav_format.ml

outputs = \
	outputs/output.ml \
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

io = \
	io/udp_io.ml \
	$(if $(W_SRT),io/srt_io.ml) \
	$(if $(W_OSS),io/oss_io.ml) \
	$(if $(W_PULSEAUDIO),io/pulseaudio_io.ml) \
	$(if $(W_PORTAUDIO),io/portaudio_io.ml) \
	$(if $(W_ALSA),io/alsa_io.ml) \
	$(if $(W_GSTREAMER),io/gstreamer_io.ml) \
	$(if $(W_FFMPEG),io/ffmpeg_filter_io.ml) \
	$(if $(W_FFMPEG),io/ffmpeg_io.ml)

tools_metadata = \
	$(if $(W_CAMOMILE),tools/metadata/metadataCharEncoding.ml,tools/metadata/no-camomile/metadataCharEncoding.ml) \
	tools/metadata/metadataBase.ml \
	tools/metadata/metadataID3v1.ml tools/metadata/metadataID3v2.ml \
	tools/metadata/metadataJPEG.ml tools/metadata/metadataPNG.ml \
	tools/metadata/metadataAVI.ml tools/metadata/metadataMP4.ml \
	tools/metadata/metadata.ml

tools = \
	tools/log.ml tools/pos.ml \
	tools/extralib.ml tools/lifecycle.ml \
	tools/stringView.ml tools/strings.ml tools/utils.ml \
	tools/runtime_error.ml tools/extra_args.ml \
	tools/json_base.ml tools/json_parser.ml tools/json_lexer.ml tools/json.ml \
        tools/doc.ml tools/plug.ml \
	tools/rqueue.ml tools/unifier.ml \
	$(if $(W_FFMPEG),tools/liqavdevice.ml) \
	$(if $(W_LAME_DYN),tools/lame_dynlink.ml) \
	$(if $(W_FDKAAC_DYN),tools/fdkaac_dynlink.ml) \
	tools/wav_aiff.ml tools/tutils.ml \
	tools/file_watcher.ml tools/file_watcher_mtime.ml \
	$(if $(W_DYNLINK),tools/dyntools.ml) \
	configure.ml tools/console.ml tools/process_handler.ml \
	tools/http.ml tools/srt_parser.ml \
	$(tools_metadata) \
        $(if $(W_MEMTRACE),tools/liqmemtrace.ml) \
	tools/liqcurl.ml \
	$(if $(W_SSL),tools/https.ml) \
	$(if $(W_OSX_SECURE_TRANSPORT),tools/https_secure_transport.ml) \
	tools/pool.ml tools/sha1.ml tools/websocket.ml \
	$(if $(W_INOTIFY),tools/file_watcher_inotify.ml) \
	$(if $(W_PROMETHEUS),tools/liq_prometheus.ml)

stream = \
	stream/frame_settings.ml stream/content.ml \
        stream/frame.ml stream/aFrame.ml stream/vFrame.ml stream/mFrame.ml \
        stream/generator.ml stream/kind.ml \
        $(if $(W_FFMPEG), tools/ffmpeg_utils.ml stream/ffmpeg_content_base.ml stream/ffmpeg_copy_content.ml stream/ffmpeg_raw_content.ml) \
        $(if $(W_GSTREAMER),tools/gstreamer_utils.ml)

visualization = \
	visualization/midimeter.ml \
	visualization/video_volume.ml \
	$(if $(W_GRAPHICS),visualization/vis_volume.ml)

synth = synth/keyboard.ml synth/synth_op.ml \
	$(if $(W_DSSI),synth/dssi_op.ml) \
	$(if $(W_SDL),synth/keyboard_sdl.ml)

builtins = \
	builtins/builtins_lang.ml builtins/builtins_runtime.ml \
	builtins/builtins_ref.ml builtins/builtins_socket.ml \
        builtins/builtins_bool.ml builtins/builtins_list.ml \
        builtins/builtins_string.ml builtins/builtins_regexp.ml \
        builtins/builtins_json.ml \
        builtins/builtins_null.ml builtins/builtins_settings.ml \
        builtins/builtins_time.ml builtins/builtins_callbacks.ml \
        builtins/builtins_server.ml builtins/builtins_math.ml \
	builtins/builtins_files.ml builtins/builtins_resolvers.ml \
        builtins/builtins_thread.ml builtins/builtins_process.ml \
        builtins/builtins_source.ml builtins/builtins_getter.ml \
        builtins/builtins_profiler.ml builtins/builtins_eval.ml \
	builtins/builtins_http.ml \
        builtins/builtins_harbor.ml \
        $(if $(W_SSL),builtins/builtins_harbor_ssl.ml) \
        $(if $(W_OSX_SECURE_TRANSPORT),builtins/builtins_harbor_secure_transport.ml) \
        $(if $(W_LO),builtins/builtins_lo.ml) \
        $(if $(W_MAGIC),builtins/builtins_magic.ml) \
        $(if $(W_CRY),builtins/builtins_cry.ml) \
        $(if $(W_LASTFM),builtins/builtins_lastfm.ml) \
        $(if $(W_PROMETHEUS),builtins/builtins_prometheus.ml) \
        builtins/builtins_ffmpeg_base.ml \
        $(if $(W_FFMPEG),builtins/builtins_ffmpeg_encoder.ml builtins/builtins_ffmpeg_decoder.ml builtins/builtins_ffmpeg_filters.ml builtins/builtins_ffmpeg_bitstream_filters.ml) \
	$(if $(W_IRC),builtins/builtins_irc.ml) \
	$(if $(W_MEM_USAGE),builtins/builtins_mem_usage.ml)

liquidsoap_sources = \
	$(tools) converters/audio_converter.ml $(stream) \
	$(if $(W_FFMPEG),tools/ffmpeg_avfilter_utils.ml) \
        converters/video_converter.ml \
	decoder/decoder_utils.ml decoder/decoder.ml \
        tools/liq_time.ml \
        request.ml tools/server.ml \
        source.ml clock.ml playlist_parser.ml \
	$(encoder_formats) encoder.ml \
	tools/server_builtins.ml \
	$(if $(W_SDL),tools/sdl_utils.ml) \
	$(if $(W_LASTFM),tools/liqfm.ml) \
	$(if $(W_ALSA), alsa_settings.ml)

liquidsoap_sources += \
	lang/type.ml lang/repr.ml lang/typing.ml \
	lang/profiler.ml lang/term.ml lang/value.ml \
	lang/lang_encoder.ml $(lang_encoders) \
	lang/environment.ml lang/typechecking.ml \
	lang/documentation.ml lang/lang_core.ml \
        lang/evaluation.ml lang/error.ml \
	lang/lang_error.ml lang/lang_source.ml \
        lang/lang.ml lang/modules.ml \
	lang/parser_helper.ml lang/parser.ml lang/lexer.ml \
	lang/preprocessor.ml lang/runtime.ml \
	builtins/builtins_request.ml \
	tools/child_support.ml \
	tools/start_stop.ml tools/ioRing.ml \
	tools/icecast_utils.ml tools/avi.ml \
	$(video_converters) $(audio_converters) $(protocols) \
	$(sources) $(outputs) tools/producer_consumer.ml \
	$(conversions) $(operators) \
	$(encoders) $(decoders) $(ogg_demuxer) $(ogg_muxer) \
	$(playlists) $(visualization) $(synth) $(io) \
	shebang.ml $(builtins) main.ml \
	$(if $(W_WINSVC),runner_service.ml,runner.ml)

# Specific rules

include ../Makefile.defs

export OCAMLPATH := $(OCAMLPATH)

OCAMLDEP_FLAGS = $(patsubst %,-I %,$(INCDIRS))
OCAML_CFLAGS = -thread $(OCAMLDEP_FLAGS)
_OCAML_CFLAGS = $(liquidsoap_ocamlcflags)
_OCAML_LFLAGS = $(liquidsoap_ocamllflags)

liquidsoap_mly = $(wildcard $(liquidsoap_sources:.ml=.mly))
liquidsoap_headers = $(wildcard $(liquidsoap_sources:.ml=.mli)) $(liquidsoap_mly:.mly=.mli)

all: liquidsoap$(EXEEXT)

liquidsoap_doc_sources = $(liquidsoap_sources)

_OCAML_DFLAGS = $(shell echo '$(_OCAML_CFLAGS) $(OCAML_CFLAGS)' | perl -pe '$$opt = "" ; s/"[^"]*"//g ; s/(-I\s*\S+|-package\s*\S+)/my $$a = $$1 ; if ($$a =~ m:-package\s*(\S)(\S+):) { $$opt="$$opt $$a -hide ".(uc $$1).$$2 } else { $$opt="$$opt $$a" }/eg ; $$_ = $$opt')
_OCAML_DFILES = $(shell echo $(liquidsoap_doc_sources) | perl -pe 's/[^\s]+\.[co]//g')
_OCAML_DFILES_H = $(wildcard $(_OCAML_DFILES:.ml=.mli))

doc: $(liquidsoap_doc_sources)
	mkdir -p ../autodoc/liquidsoap
	$(OCAMLFIND) ocamldoc -stars -sort -html -d ../autodoc/liquidsoap -t Liquidsoap -I +threads $(OCAML_DFLAGS) $(_OCAML_DFLAGS) $(_OCAML_DFILES) $(_OCAML_DFILES_H)

clean:
	@echo "Cleaning..."
	@rm -f **/.cm* $(liquidsoap_sources:.ml=.$(o)) $(liquidsoap_sources:.ml=.$(i)) $(liquidsoap_c_files:.c=.o) depend liquidsoap.$(a)
	@$(MAKE) -C test $@

DISTFILES = Makefile META.in $(shell find . -name \*.mli) $(shell find . -name \*.ml) $(shell find . -name \*.c)

dist:
	mkdir $(DISTDIR)/src
	rsync -amR $(DISTFILES) $(DISTDIR)/src

install:
	$(INSTALL) -d $(bindir)
	$(INSTALL_PROGRAM) liquidsoap$(EXEEXT) $(bindir)
ifneq ($(BYTE),)
  ifneq ($(NO_CUSTOM),)
	$(OCAMLFIND) install liquidsoap META dllliquidsoap.so
  else
	$(OCAMLFIND) install liquidsoap META
  endif
endif

configure.ml:
	@echo
	@echo " ERROR Missing src/configure.ml, Please run ./configure first!"
	@if [ -f ../configure-with-options ] ; then \
	  echo "       Note that you can source configure-with-options" ; \
	  echo "       for running configure with the same options as last time." ; \
	fi
	@echo
	@exit 1

depend: $(liquidsoap_sources) $(liquidsoap_headers)
	$(V)echo OCAMLDEP
	$(V)$(OCAMLDEP) $(_OCAMLDEP_FLAGS) $(OCAMLDEP_FLAGS) $(liquidsoap_sources) $(liquidsoap_headers) > $@

include depend

dllliquidsoap.so: $(liquidsoap_c_files:.c=.o)
	$(V)echo OCAMLMKLIB -o dllliquidsoap.so
	$(V)$(OCAMLMKLIB) -o liquidsoap $(liquidsoap_c_files:.c=.o)

ifeq ($(BYTE),yes)
  ifeq ($(NO_CUSTOM),yes)
      c_objs=dllliquidsoap.so $(liquidsoap_c_files:.c=.o)
      c_link=-dllib dllliquidsoap.so
  else
    c_objs=$(liquidsoap_c_files:.c=.o)
    c_link=-custom $(liquidsoap_c_files:.c=.o)
  endif
else
  c_objs=$(liquidsoap_c_files:.c=.o)
  c_link=$(c_objs)
endif

liquidsoap$(EXEEXT): $(liquidsoap_sources:.ml=.$(o)) $(c_objs)
	$(V)echo $(OCAMLCOMP) -o liquidsoap$(EXEEXT)
	$(V)$(OCAMLC) -o liquidsoap$(EXEEXT) $(_OCAML_CFLAGS) $(OCAML_CFLAGS) $(_OCAML_LFLAGS) $(OCAML_LFLAGS) $(liquidsoap_sources:.ml=.$(o)) $(c_link)

lib: liquidsoap.cmxa

liquidsoap_lib_sources = $(filter-out main.ml runner.ml, $(liquidsoap_sources))

liquidsoap.$(a): $(liquidsoap_sources:.ml=.$(o)) $(c_objs)
	$(OCAMLC) -a -o $@ $(_OCAML_CFLAGS) $(OCAML_CFLAGS) $(OCAML_LFLAGS) $(liquidsoap_lib_sources:.ml=.$(o)) $(c_link)

# Generic rules

%.ml %.mli: %.mly
	@echo $(<:.mly=.mli) $(<:.mly=.ml) $(<:.mly=.conflicts) >> auto_clean
	$(V)echo MENHIR $<
	$(V)$(MENHIR) --unused-tokens --explain $<

%.$(o): %.ml
	$(V)echo $(OCAMLCOMP) -c $<
	$(V)$(OCAMLC) $(_OCAML_CFLAGS) $(OCAML_CFLAGS) -c $<

%.$(i): %.mli
	$(V)echo $(OCAMLCOMP) -c $<
	$(V)$(OCAMLC) $(_OCAML_CFLAGS) $(OCAML_CFLAGS) -c $<

%.o: %.c
	$(V)echo CC -c $<
	$(V)$(CC) $(CFLAGS) -c $< -o $@
