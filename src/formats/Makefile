
DISTFILES = $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.c) Makefile

top_srcdir= ../..
include $(top_srcdir)/Makefile.rules

id3tag_stubs.o: id3tag_stubs.c
	$(CC) -c -g -Wall -I$(shell $(OCAMLC) -where) $<

test_id3tag: id3tag_stubs.o id3tag.mli id3tag.ml test_id3tag.ml
	ocamlc id3tag.mli
	ocamlopt -c id3tag.ml
	ocamlopt unix.cmxa id3tag.cmx id3tag_stubs.o test_id3tag.ml -cclib -lid3tag -o test_id3tag
