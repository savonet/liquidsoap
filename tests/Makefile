.PHONY: test test_files test_stereo test_mono test_stream_audio test_stream_video test_ffmpeg_audio_decoder test_ffmpeg_video_decoder test_gstreamer_audio_decoder test_gstreamer_video_decoder

DISTFILES = Makefile mk_stream_test.sh $(wildcard *.liq) $(wildcard *.pl)
SUBDIRS = files fixme generic language regression

top_srcdir = ..

test:
	@$(MAKE) -C files
	@$(MAKE) -C language
	@$(MAKE) -C regression
	@$(MAKE) -C generic
	@./run_test.sh /usr/bin/perl ./type_errors.pl
	@echo
	@echo
	@echo "***** Performance tests *****"
	@./performance.liq

include $(top_srcdir)/Makefile.rules
