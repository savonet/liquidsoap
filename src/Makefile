
SUBDIRS= operators sources tools formats protocols outputs plugins

DISTFILES = $(wildcard *.mli) Makefile liquidtts.in \
			main.ml root.ml types.ml decoder.ml request.ml

ocaml_progs= liquidsoap liquidtts

formats = \
	formats/ogg.ml formats/wavformat.ml \
	$(if $(W_ID3TAG),formats/id3tag_stubs.c formats/id3tag.ml) \
	$(if $(W_ID3TAG),formats/id3tag_plug.ml) \
	$(if $(W_MP3ID3),formats/mp3id3_plug.ml) \
	$(if $(W_MP3),formats/mp3.ml)

protocols = \
	protocols/say.ml protocols/annotate.ml \
	protocols/extproto.ml

sources = \
	sources/blank.ml sources/request_source.ml sources/sine.ml \
	sources/req_simple.ml sources/playlist.ml \
	sources/req_queue.ml sources/req_equeue.ml sources/http.ml \
	$(if $(W_ALSA),sources/alsa_in.ml) \
	$(if $(W_RTP),sources/rtp_input.ml)

operators = \
	operators/switch.ml operators/fade.ml operators/add.ml \
	operators/setvol.ml operators/rewrite_metadata.ml operators/on_metadata.ml \
	operators/store_metadata.ml \
	operators/delay.ml operators/filter.ml operators/say_metadata.ml \
	operators/noblank.ml operators/mixing_table.ml operators/sequence.ml

outputs = outputs/output.ml outputs/icecast2.ml outputs/vorbis_encoded.ml \
	$(if $(W_LAME),outputs/lame_encoded.ml) \
	$(if $(W_ALSA),outputs/alsa_out.ml) outputs/wavfile.ml \
	$(if $(W_RTP),outputs/rtp_output.ml)

tools = tools/doc.ml tools/plug.ml tools/utils.ml \
	tools/tutils.ml \
	tools/rqueue.ml \
	tools/mixer.ml tools/mixer_c.c \
	tools/wav.ml $(if $(W_RTP),tools/rtp_c.o tools/rtp.ml) \
	$(if $(BYTE),tools/dynliq.ml)

liquidsoap_sources= $(tools) configure.ml \
	decoder.ml request.ml types.ml root.ml tools/server.ml \
	tools/lang.ml \
	tools/lang_lexer.ml \
	tools/lang_parser.ml \
	tools/lang_user.ml \
	$(formats) $(protocols) $(sources) $(operators) $(outputs) \
	tools/lang_builtins.ml \
	main.ml

OCAML_CFLAGS= -thread \
			  -I tools -I outputs -I formats -I sources -I operators
OCAML_LFLAGS= $(if $(BYTE),dynlink.cma) $(if $(W_RTP),-cclib -lortp) \
			  $(if $(W_ID3TAG),-cclib -lid3tag)
DEP_OPTS= -I sources -I operators -I tools -I formats -I outputs

liquidsoap_doc_sources= \
	types.ml root.ml \
	tools/plug.ml \
	decoder.ml request.ml outputs/output.ml \
	tools/lang.ml tools/tutils.ml \
	tools/doc.ml tools/mixer.ml tools/wav.ml \
	$(if $(W_RTP),tools/rtp.ml) \
	sources/request_source.ml operators/switch.ml

top_srcdir=..
include $(top_srcdir)/Makefile.rules

tools/rtp_c.o: tools/rtp_c.c
	make -C tools rtp_c.o
