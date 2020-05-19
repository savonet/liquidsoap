.PHONY: test

DISTFILES = Makefile $(wildcard *.sh) $(wildcard *.liq) $(wildcard *.pl)
SUBDIRS = fixme media language regression

top_srcdir = ..

test:
	@$(MAKE) -C language test
	@$(MAKE) -C media test
	@$(MAKE) -C regression test
	@./run_test.sh /usr/bin/perl ./type_errors.pl
	@echo
	@echo
	@echo "***** Performance tests *****"
	@./performance.liq

include $(top_srcdir)/Makefile.rules
