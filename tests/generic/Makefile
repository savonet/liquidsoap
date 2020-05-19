DISTFILES = Makefile $(wildcard *.liq *.liq.in)
top_srcdir = ../..

all: files test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder

files:
	$(MAKE) -C ../files

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

.PHONY: files

include ../Makefile.include
