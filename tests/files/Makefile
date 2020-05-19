top_srcdir = ../..

default: test_files

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

ENCODED_AUDIO_FILES=$(AUDIO_TEST_FORMATS:%=audio/%)

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

ENCODED_VIDEO_FILES=$(VIDEO_TEST_FORMATS:%=video/%)

ENCODED_FILES=$(ENCODED_AUDIO_FILES) $(ENCODED_VIDEO_FILES)

$(ENCODED_FILES): ENCODER = $(subst @,%,$(word 1,$(subst ., ,$*)))

test_files: audio video $(ENCODED_FILES)

audio:
	@mkdir -p files/audio

video:
	@mkdir -p files/video

audio/%: $(top_srcdir)/src/liquidsoap
	$(call generate_audio,$(ENCODER),$@)

video/%: $(top_srcdir)/src/liquidsoap
	$(call generate_video,$(ENCODER),$@)

clean-local:
	rm -rf audio video

include $(top_srcdir)/Makefile.rules
