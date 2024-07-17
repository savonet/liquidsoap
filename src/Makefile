
SUBDIRS= operators sources tools formats protocols outputs

DISTFILES = $(wildcard *.mli) Makefile liquidtts.in \
			main.ml root.ml types.ml decoder.ml request.ml

ocaml_progs= liquidsoap liquidtts

formats = \
	formats/ogg.ml $(if $(W_MP3),formats/mp3.ml) formats/wavformat.ml

protocols = \
	protocols/say.ml $(if $(W_FETCH),protocols/fetchlib.ml) \
	$(if $(W_MYSQL),protocols/strider.ml) \
	$(if $(W_STRIDER),protocols/striderlib.ml)

sources = \
	sources/blank.ml sources/play_files.ml sources/sine.ml \
	sources/one_file.ml sources/playlist.ml \
	sources/external_request.ml \
	sources/telnet_request.ml $(if $(W_SAVONET),sources/savonet_request.ml)

operators = \
	operators/switch.ml operators/fade.ml operators/add.ml \
	operators/setvol.ml operators/random_switch.ml \
	operators/delay.ml operators/filter.ml operators/metadatas.ml

outputs = \
	outputs/output.ml \
	outputs/wavfile.ml outputs/oggfile.ml \
	outputs/icecast2.ml outputs/ping.ml $(if $(W_RTP),outputs/rtp_output.ml)

tools = tools/doc.ml tools/plug.ml tools/tutils.ml \
	tools/scheduling_defs.ml \
	tools/scheduling_parser.ml tools/scheduling_lexer.ml \
	tools/scheduling.ml \
	tools/mixer.ml tools/mixer_c.c \
	tools/wav.ml $(if $(W_RTP),tools/rtp_c.o tools/rtp.ml)

liquidsoap_sources= $(tools) configure.ml \
	decoder.ml request.ml types.ml $(outputs) root.ml \
	$(if $(W_SAVONET),tools/savonet_client.ml) \
	tools/lang.ml \
	tools/lang_lexer.ml \
	tools/lang_parser.ml \
	tools/lang_user.ml \
	$(formats) $(protocols) $(sources) $(operators) \
	main.ml

OCAML_CFLAGS= -thread -I tools -I outputs -I formats -I sources -I operators \
			  -package dtools -package vorbis -package shout \
			  $(if $(W_MP3),-package mad -package mp3id3) \
              $(if $(W_FETCH),-package fetch) \
              $(if $(W_MYSQL),-package mysql) \
              $(if $(W_STRIDER),-package strider) \
              $(if $(W_SAVONET),-package savonet)
OCAML_LFLAGS= $(if $(W_RTP),-cclib -lortp)
DEP_OPTS= -I sources -I operators -I tools -I formats -I outputs

liquidsoap_doc_sources= \
	types.ml root.ml \
	tools/plug.ml \
	decoder.ml request.ml outputs/output.ml \
	tools/lang.ml tools/tutils.ml \
	tools/doc.ml tools/scheduling.ml tools/mixer.ml tools/wav.ml $(if $(W_RTP),tools/rtp.ml) \
	sources/play_files.ml sources/external_request.ml operators/switch.ml

top_srcdir=..
include $(top_srcdir)/Makefile.rules

tools/rtp_c.o: tools/rtp_c.c
	make -C tools rtp_c.o
