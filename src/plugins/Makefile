
DISTFILES= $(wildcard *.ml.example) Makefile

OCAML_CFLAGS= -thread -I .. -I ../tools -I ../outputs -I ../formats \
			  -I ../sources -I ../operators
OCAML_LFLAGS= dynlink.cma
dep_opts= -I ../sources -I ../operators -I ../tools -I ../formats -I ../outputs

top_srcdir=../..
include $(top_srcdir)/Makefile.rules

ML=$(wildcard *.ml)
plugins: $(ML:.ml=.cmo)
