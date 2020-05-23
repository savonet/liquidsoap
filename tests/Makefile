.PHONY: test

DISTFILES = Makefile $(wildcard *.sh) $(wildcard *.liq) $(wildcard *.pl)
SUBDIRS = fixme media language regression

top_srcdir = ..

test:
	@$(MAKE) -C language $@
	@$(MAKE) -C streams $@
	@$(MAKE) -C media $@
	@$(MAKE) -C regression $@
	@./run_test.sh /usr/bin/perl ./type_errors.pl
	@echo
	@echo
	$(MAKE) performance

performance: performance.liq
	@echo "***** Performance tests *****"
	@./performance.liq

perf:
	perf record --call-graph=dwarf -- ../src/liquidsoap ../libs/pervasives.liq performance.liq
	perf report

include $(top_srcdir)/Makefile.rules
