DISTFILES = Makefile parser.mly $(wildcard *.ml) $(wildcard *.mli)
top_srcdir=../..
include $(top_srcdir)/Makefile.rules

menhir:
	@rm -f parser.conflicts
	menhir --explain parser.mly
	@cat parser.conflicts

.PHONY: menhir
