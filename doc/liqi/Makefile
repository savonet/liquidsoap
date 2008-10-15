DISTFILES= $(wildcard *.ml*) Makefile

ocaml_progs=liqi
liqi_sources=liqi.ml html.ml latex.ml liqi_parser.ml liqi_lexer.ml main.ml

# Stupid need because of liquidsoap's CFLAGS, don't care for now
OCAML_CFLAGS=-thread

top_srcdir=../..
include $(top_srcdir)/Makefile.rules
