DISTFILES = $(wildcard *.ml) $(wildcard *.camlp4) $(wildcard *.mli) $(wildcard *.c) $(wildcard *.in) Makefile

ocaml_tests = mixer_test wav_test tutils_test
OCAML_CFLAGS= -thread -package dtools

mixer_test_sources = mixer_c.c mixer.ml mixer_test.ml
wav_test_sources = mixer_c.c mixer.ml wav.ml wav_test.ml
tutils_test_sources = tutils.ml tutils_test.ml

top_srcdir=../..
include $(top_srcdir)/Makefile.rules

sha1: sha1.ml
	ocamlopt -annot sha1.ml -o sha1
	./sha1