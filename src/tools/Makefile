
DISTFILES = $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.c) Makefile \
	lang_parser.mly lang_lexer.mll

ocaml_tests = mixer_test $(if $(W_RTP),rtp_send) wav_test tutils_test
OCAML_CFLAGS= -thread $(if $(W_RTP),-cclib -lortp) -package dtools

mixer_test_sources = mixer_c.c mixer.ml mixer_test.ml
rtp_send_sources = mixer_c.c mixer.ml rtp_c.o rtp.ml rtp_send.ml
wav_test_sources = mixer_c.c mixer.ml wav.ml wav_test.ml
tutils_test_sources = tutils.ml tutils_test.ml

top_srcdir=../..
include $(top_srcdir)/Makefile.rules

rtp_c.o: rtp_c.c
	$(CC) -g -DINET6 -Wall $(shell pkg-config --cflags glib-2.0) \
	-I$(shell $(OCAMLC) -where) -I$(ORTP_INCLUDE) $< -c
