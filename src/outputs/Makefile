
DISTFILES = $(wildcard *.ml) $(wildcard *.mli) Makefile

ocaml_progs = $(if $(W_RTP),soaptube)

soaptube_sources= ../configure.ml \
  ../tools/doc.ml ../tools/plug.ml \
  ../tools/mixer_c.c ../tools/mixer.ml \
  ../tools/rtp_c.o ../tools/rtp.ml \
  output.ml \
  ../tools/wav.ml wavfile.ml \
  oggfile.ml icecast2.ml ping.ml rtp_output.ml \
  soaptube.ml

OCAML_CFLAGS = -thread -I .. -I ../tools -package dtools -package vorbis -package shout
OCAML_LFLAGS = -cclib -lortp

top_srcdir= ../..
include $(top_srcdir)/Makefile.rules

# A special dirty rule because the gcc call is $cwd relative
../tools/rtp_c.o: ../tools/rtp_c.c
	make -C ../tools rtp_c.o
