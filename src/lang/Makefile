DISTFILES = Makefile parser.mly $(wildcard *.ml) $(wildcard *.mli)
top_srcdir=../..
include $(top_srcdir)/Makefile.rules

menhir:
	@rm -f lang_parser.conflicts
	menhir --explain parser.mly
	@cat lang_parser.conflicts

.PHONY: menhir
