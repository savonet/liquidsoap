.PHONY: test test_files test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder

DISTFILES = Makefile mk_stream_test.sh $(wildcard *.liq) $(wildcard *.pl)
SUBDIRS = fixme tests generic
LIQ = $(sort $(wildcard tests/*.liq))
TESTS = $(basename $(LIQ))

top_srcdir = ..

test: test_files test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder $(TESTS)
	@./run_test.sh /usr/bin/perl ./type_errors.pl
	@echo
	@echo
	@echo "***** Performance tests *****"
	@./performance.liq

tests/%: tests/%.liq
	@./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" $<

define generate_audio
	@/bin/bash -c 'echo -e "Generating \033[1m$2\033[0m... "'
	@$(top_srcdir)/src/liquidsoap  --no-pervasives -q $(top_srcdir)/libs/pervasives.liq \
          "clock.assign_new(sync='none',[ \
          output.file($1,'$2',fallible=true,on_stop=shutdown, \
          once(sine(duration=2.)))])"
endef

# % is too annoying to deal with in variables so
# let's use @ and substitute.
AUDIO_TEST_FORMATS = \
  @flac(stereo).flac \
  @flac(mono).flac \
  @wav(stereo).wav \
  @wav(mono).wav \
  @mp3(mono).mp3 \
  @mp3(stereo).mp3 \
  @ogg(@vorbis(mono)).ogg \
  @ogg(@vorbis(stereo)).ogg \
  @ogg(@flac(mono)).ogg \
  @ogg(@flac(stereo)).ogg \
  @ogg(@opus(mono)).ogg \
  @ogg(@opus(stereo)).ogg

# TODO: fix speex or officially deprecate it?
#  @ogg(@speex(mono)).ogg \
#  @ogg(@speex(stereo)).ogg

ENCODED_AUDIO_FILES=$(AUDIO_TEST_FORMATS:%=files/audio/%)

define generate_video
        @/bin/bash -c 'echo -e "Generating \033[1m$2\033[0m... "'
        @$(top_srcdir)/src/liquidsoap  --no-pervasives -q $(top_srcdir)/libs/pervasives.liq \
          'clock.assign_new(sync="none",[ \
          output.file($(subst \,,$1),"$2",fallible=true,on_stop=shutdown, \
          once(noise(duration=0.5)))])'
endef

VIDEO_TEST_FORMATS = \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=1),@video(codec=\"libx264\")).mp4 \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=2),@video(codec=\"libx264\")).mp4 \
  @ffmpeg(format=\"mp4\",@audio(codec=none),@video(codec=\"libx264\")).mp4

ENCODED_VIDEO_FILES=$(VIDEO_TEST_FORMATS:%=files/video/%)

ENCODED_FILES=$(ENCODED_AUDIO_FILES) $(ENCODED_VIDEO_FILES)

$(ENCODED_FILES): ENCODER = $(subst @,%,$(word 1,$(subst ., ,$*)))

test_files: files/audio files/video $(ENCODED_FILES)

files/audio:
	@mkdir -p files/audio

files/video:
	@mkdir -p files/video

files/audio/%: $(top_srcdir)/src/liquidsoap
	$(call generate_audio,$(ENCODER),$@)

files/video/%: $(top_srcdir)/src/liquidsoap
	$(call generate_video,$(ENCODER),$@)

test_mono: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" generic/test_mono.liq "Mono decoding test for $$i" || exit; \
	done

test_stereo: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" generic/test_stereo.liq "Stereo decoding test for $$i" || exit; \
	done

test_stream_audio: $(top_srcdir)/src/liquidsoap
	@for i in $(AUDIO_TEST_FORMATS:%="%"); do \
	  ./mk_stream_test.sh $$i audio; \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" generic/test_stream_audio.liq "Audio stream decoder test for $$i" || exit; \
	done

test_stream_video: $(top_srcdir)/src/liquidsoap
	@for i in $(VIDEO_TEST_FORMATS:%="%"); do \
	  ./mk_stream_test.sh $$i video; \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" generic/test_stream_video.liq "Video stream decoder test for $$i" || exit; \
	done

test_ffmpeg_audio_decoder: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" generic/test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for $$i" || exit; \
	done

test_ffmpeg_video_decoder: $(ENCODED_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" generic/test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for $$i" || exit; \
	done

# See: https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/555
test_gstreamer_video_decoder: $(ENCODED_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  if echo $$i | grep -v 'codec=none' > /dev/null 2>&1; then \
	    ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" generic/test_gstreamer_video_decoder.liq "Gstreamer video decoder test for $$i" || exit; \
          fi \
	done

clean-local:
	rm -rf files

include $(top_srcdir)/Makefile.rules
