DISTFILES = $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.c) $(wildcard *.in) Makefile

top_srcdir=../../..
include $(top_srcdir)/Makefile.rules

build:
	@dune build

test:
	@dune test
