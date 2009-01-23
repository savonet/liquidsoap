DISTFILES = $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.c) Makefile

top_srcdir=../..
include $(top_srcdir)/Makefile.rules

opal_io_c.o: opal_io_c.c
	$(CC) $(CFLAGS) -c -g -Wall -I$(shell $(OCAMLC) -where) -I$(OPAL_INCLUDE) $<
