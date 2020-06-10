.PHONY: test

DISTFILES = Makefile $(wildcard *.sh) $(wildcard *.liq) $(wildcard *.pl)
SUBDIRS = fixme media language regression

top_srcdir = ..

test:
	@$(MAKE) -C language $@
	@$(MAKE) -C streams $@
	@$(MAKE) -C media $@
	@$(MAKE) -C regression $@
	@echo
	@echo
	@$(MAKE) -C performance $@

include $(top_srcdir)/Makefile.rules
