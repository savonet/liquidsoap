.PHONY: test_encoder test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_filter test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder

.NOTPARALLEL: test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder

DISTFILES = Makefile $(wildcard *.sh) $(wildcard *.liq *.liq.in *.json)
top_srcdir = $(shell realpath ../..)

test: test_encoder test_stereo test_mono test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_filter test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder test_ffmpeg_raw_hls test_ffmpeg_inline_encode_decode_audio test_ffmpeg_inline_encode_decode_video test_ffmpeg_inline_encode_decode test_ffmpeg_distributed_hls # test_stream_audio test_stream_video

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
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\"),@videoDOTnone).mp4

# TODO: fix speex or officially deprecate it?
#  @ogg(@speex(mono)).ogg \
#  @ogg(@speex(stereo)).ogg

AUDIO_VIDEO_TEST_FORMATS = \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=1),@video(codec=\"libx264\")).mp4 \
  @ffmpeg(format=\"mp4\",@audio(codec=\"aac\",channels=2),@video(codec=\"libx264\")).mp4

VIDEO_ONLY_TEST_FORMATS = \
  @ffmpeg(format=\"mp4\",@audioDOTnone,@video(codec=\"libx264\")).mp4

VIDEO_TEST_FORMATS = $(AUDIO_VIDEO_TEST_FORMATS) $(AUDIO_ONLY_TEST_FORMATS) $(VIDEO_ONLY_TEST_FORMATS)

ENCODED_AUDIO_FILES=$(AUDIO_TEST_FORMATS:%=files/audio/%)

ENCODED_AUDIO_VIDEO_FILES=$(AUDIO_VIDEO_TEST_FORMATS:%=files/video/%)

ENCODED_VIDEO_FILES=$(VIDEO_TEST_FORMATS:%=files/video/%)

ENCODED_FILES=$(ENCODED_AUDIO_FILES) $(ENCODED_VIDEO_FILES)

$(ENCODED_FILES): ENCODER = $(subst @,%,$(word 1,$(subst ., ,$*)))

define generate_audio
	@$(eval PREFIX := $(shell /bin/bash -c "echo $$RANDOM"))
        @./mk_encoder_test.sh "$1" sine $(PREFIX)
        @../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $2" media/files/$(PREFIX)/test_encoder.liq "$1 encoder" || exit
endef

define generate_video
	@$(eval PREFIX := $(shell /bin/bash -c "echo $$RANDOM"))
        @./mk_encoder_test.sh "$1" noise $(PREFIX)
        @../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $2" media/files/$(PREFIX)/test_encoder.liq "$1 encoder" || exit
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
	  if echo $$i | grep -v 'ffmpeg' > /dev/null 2>&1; then \
	    ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_mono.liq "Mono decoding test for $$i" || exit; \
	  fi \
	done

test_stereo: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  if echo $$i | grep -v 'ffmpeg' > /dev/null 2>&1; then \
	    ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_stereo.liq "Stereo decoding test for $$i" || exit; \
	  fi \
	done

test_ffmpeg_filter: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $<" media/test_ffmpeg_filter.liq "FFmpeg filter test for $$i" || exit; \
	done

test_ffmpeg_audio_decoder: $(ENCODED_AUDIO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for $$i" || exit; \
	done

test_ffmpeg_video_decoder: $(ENCODED_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for $$i" || exit; \
	done

test_ffmpeg_copy_decoder: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_copy_decoder.liq "FFmpeg copy decoder test for $$i" || exit; \
	done

test_ffmpeg_copy_and_encode_decoder: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_copy_and_encode_decoder.liq "FFmpeg copy+encode decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_decoder: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_decoder.liq "FFmpeg raw decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_and_encode_decoder: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_and_encode_decoder.liq "FFmpeg raw+encode decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_and_copy_decoder: $(ENCODED_AUDIO_VIDEO_FILES) $(top_srcdir)/src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_and_copy_decoder.liq "FFmpeg raw+copy decoder test for $$i" || exit; \
	done

test_ffmpeg_inline_encode_decode:
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode.liq

test_ffmpeg_inline_encode_decode_audio:
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode_audio.liq

test_ffmpeg_inline_encode_decode_video:
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode_video.liq

test_ffmpeg_distributed_hls: 
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" media/test_ffmpeg_distributed_hls.liq

test_ffmpeg_raw_hls:
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" media/test_ffmpeg_raw_hls.liq

include $(top_srcdir)/Makefile.rules

clean:
	rm -rf files/
