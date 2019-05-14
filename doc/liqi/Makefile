DISTFILES= Makefile liqi_parser.mly liqi_lexer.mll $(wildcard *.ml)

ocaml_progs=liqi
liqi_sources=liqi.ml html.ml latex.ml markdown.ml liqi_parser.ml liqi_lexer.ml main.ml

# Stupid need because of liquidsoap's CFLAGS, don't care for now
liqi_ocamlcflags=-thread -linkpkg -package pcre -package str -annot

top_srcdir=../..
include $(top_srcdir)/Makefile.rules
