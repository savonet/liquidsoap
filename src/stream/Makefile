
DISTFILES = $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.c) Makefile

top_srcdir= ../..
include $(top_srcdir)/Makefile.rules

rgb_c.o: rgb_c.c
	$(CC) $(CFLAGS) -c -O3 $<
