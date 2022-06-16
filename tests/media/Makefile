.PHONY: test_encoder test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_filter test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder test_ffmpeg_copy_concat test_video_size

.NOTPARALLEL: test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder test_video_size

test: test_encoder test_stereo test_mono test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_ffmpeg_filter test_ffmpeg_copy_decoder test_ffmpeg_copy_and_encode_decoder test_ffmpeg_raw_decoder test_ffmpeg_raw_and_encode_decoder test_ffmpeg_raw_and_copy_decoder test_ffmpeg_raw_hls test_ffmpeg_inline_encode_decode_audio test_ffmpeg_inline_encode_decode_video test_ffmpeg_inline_encode_decode test_ffmpeg_distributed_hls test_ffmpeg_copy_concat test_video_size test_ffmpeg_transparency_filter # test_stream_audio test_stream_video

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
        @../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $2" media/files/$(PREFIX)/test_encoder.liq "$1 encoder" || exit
endef

define generate_video
	@$(eval PREFIX := $(shell /bin/bash -c "echo $$RANDOM"))
        @./mk_encoder_test.sh "$1" noise $(PREFIX)
        @../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $2" media/files/$(PREFIX)/test_encoder.liq "$1 encoder" || exit
endef

test_encoder: files/audio files/video $(ENCODED_FILES)

files/audio:
	@mkdir -p $@

files/video:
	@mkdir -p $@

files/audio/%: ../../src/liquidsoap
	$(call generate_audio,$(ENCODER),$@)

files/video/%: ../../src/liquidsoap
	$(call generate_video,$(ENCODER),$@)

test_mono: $(ENCODED_AUDIO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  if echo $$i | grep -v 'ffmpeg' > /dev/null 2>&1; then \
	    ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_mono.liq "Mono decoding test for $$i" || exit; \
	  fi \
	done

test_stereo: $(ENCODED_AUDIO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  if echo $$i | grep -v 'ffmpeg' > /dev/null 2>&1; then \
	    ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_stereo.liq "Stereo decoding test for $$i" || exit; \
	  fi \
	done

test_ffmpeg_filter: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $<" media/test_ffmpeg_filter.liq "FFmpeg filter test for $$i" || exit; \
	done

test_ffmpeg_add_text: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $<" media/test_ffmpeg_add_text.liq "FFmpeg add text filter test for $$i" || exit; \
	done

test_ffmpeg_audio_decoder: $(ENCODED_AUDIO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for $$i" || exit; \
	done

test_ffmpeg_video_decoder: $(ENCODED_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for $$i" || exit; \
	done

test_video_size: $(ENCODED_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_video_size.liq "FFmpeg video decoder test for $$i" || exit; \
	done

test_ffmpeg_copy_decoder: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_copy_decoder.liq "FFmpeg copy decoder test for $$i" || exit; \
	done

test_ffmpeg_copy_and_encode_decoder: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_copy_and_encode_decoder.liq "FFmpeg copy+encode decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_decoder: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_decoder.liq "FFmpeg raw decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_and_encode_decoder: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_and_encode_decoder.liq "FFmpeg raw+encode decoder test for $$i" || exit; \
	done

test_ffmpeg_raw_and_copy_decoder: $(ENCODED_AUDIO_VIDEO_FILES) ../../src/liquidsoap
	@for i in $(ENCODED_AUDIO_VIDEO_FILES:%="%"); do \
	  ../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq - -- $$i" media/test_ffmpeg_raw_and_copy_decoder.liq "FFmpeg raw+copy decoder test for $$i" || exit; \
	done

test_ffmpeg_inline_encode_decode:
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode.liq

test_ffmpeg_inline_encode_decode_audio:
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode_audio.liq

test_ffmpeg_inline_encode_decode_video:
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_inline_encode_decode_video.liq

test_ffmpeg_distributed_hls:
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_distributed_hls.liq

test_ffmpeg_raw_hls:
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_raw_hls.liq

test_taglib: files/audio/@mp3(mono).mp3
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -"  media/test_taglib.liq

files/ffmpeg_copy_concat/first-concat.mp4:
	mkdir -p files/ffmpeg_copy_concat
	ffmpeg -f lavfi -i testsrc=duration=20:size=1280x720:rate=30 -vf 'setpts=N+1235' $@

files/ffmpeg_copy_concat/second-concat.mp4:
	mkdir -p files/ffmpeg_copy_concat
	ffmpeg -f lavfi -i testsrc=duration=30:size=1280x720:rate=30 -vf 'setpts=N+756' $@

files/ffmpeg_copy_concat/third-concat.mp4:
	mkdir -p files/ffmpeg_copy_concat
	ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -vf 'setpts=N+245' $@

test_ffmpeg_copy_concat: files/ffmpeg_copy_concat/first-concat.mp4 files/ffmpeg_copy_concat/second-concat.mp4 files/ffmpeg_copy_concat/third-concat.mp4
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_copy_concat.liq

test_ffmpeg_complex_filter: files/audio/@mp3(stereo).mp3 files/audio/@mp3(mono).mp3
	 @../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -"  media/test_ffmpeg_complex_filter.liq

clean:
	rm -rf files/

files/background.jpg:
	@mkdir -p files
	ffmpeg -f lavfi -i testsrc=size=1280x720 -t 1 -f mjpeg $@

files/logo.png:
	@mkdir -p files
	ffmpeg -f lavfi -i testsrc=size=50x50 -t 1 -f mjpeg $@

test_ffmpeg_transparency_filter: files/audio/@mp3(stereo).mp3 files/background.jpg files/logo.png
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" media/test_ffmpeg_transparency_filter.liq
