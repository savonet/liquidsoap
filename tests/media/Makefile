.PHONY: test_encoder test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder

DISTFILES = Makefile $(wildcard *.sh) $(wildcard *.liq *.liq.in)
top_srcdir = $(shell realpath ../..)

test: test_encoder test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder

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
  @ogg(@opus(stereo)).ogg \
# TODO: fix speex or officially deprecate it?
#  @ogg(@speex(mono)).ogg \
#  @ogg(@speex(stereo)).ogg

VIDEO_TEST_FORMATS = \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=1),@video(codec=\"libx264\")).mp4 \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=2),@video(codec=\"libx264\")).mp4 \
  @ffmpeg(format=\"mp4\",@audio(codec=none),@video(codec=\"libx264\")).mp4

ENCODED_AUDIO_FILES=$(AUDIO_TEST_FORMATS:%=files/audio/%)

ENCODED_VIDEO_FILES=$(VIDEO_TEST_FORMATS:%=files/video/%)

ENCODED_FILES=$(ENCODED_AUDIO_FILES) $(ENCODED_VIDEO_FILES)

$(ENCODED_FILES): ENCODER = $(subst @,%,$(word 1,$(subst ., ,$*)))

define generate_audio
        @./mk_encoder_test.sh "$1" sine
        @../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $2" media/test_encoder.liq "$1 encoder" || exit
endef

define generate_video
        @./mk_encoder_test.sh "$1" noise
        @../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $2" media/test_encoder.liq "$1 encoder" || exit
endef

test_encoder: files/audio files/video $(ENCODED_FILES)

files/audio:
	@mkdir -p $@

files/video:
	@mkdir -p $@

files/audio/%: $(top_srcdir)/src/liquidsoap
	$(call generate_audio,$(ENCODER),$@)

files/video/%: $(top_srcdir)/src/liquidsoap
	$(call generate_video,$(ENCODER),$@)

test_mono: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" media/test_mono.liq "Mono decoding test for $$i" || exit; \
	done

test_stereo: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" media/test_stereo.liq "Stereo decoding test for $$i" || exit; \
	done

test_stream_audio: $(top_srcdir)/src/liquidsoap
	@for i in $(AUDIO_TEST_FORMATS:%="%"); do \
	  ./mk_stream_test.sh $$i audio; \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" media/test_stream_audio.liq "Audio stream decoder test for $$i" || exit; \
	done

test_stream_video: $(top_srcdir)/src/liquidsoap
	@for i in $(VIDEO_TEST_FORMATS:%="%"); do \
	  ./mk_stream_test.sh $$i video; \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" media/test_stream_video.liq "Video stream decoder test for $$i" || exit; \
	done

test_ffmpeg_audio_decoder: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" media/test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for $$i" || exit; \
	done

test_ffmpeg_video_decoder: $(ENCODED_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" media/test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for $$i" || exit; \
	done

# See: https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/555
test_gstreamer_video_decoder: $(ENCODED_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  if echo $$i | grep -v 'codec=none' > /dev/null 2>&1; then \
	    ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq - -- $$i" media/test_gstreamer_video_decoder.liq "Gstreamer video decoder test for $$i" || exit; \
          fi \
	done

include $(top_srcdir)/Makefile.rules
