test:
	@$(MAKE) -C language $@
	@$(MAKE) -C streams $@
	@$(MAKE) -C media $@
	@$(MAKE) -C regression $@
	@echo
	@echo
	@$(MAKE) -C performance $@

DISTFILES = \
	Makefile $(wildcard *.sh) $(wildcard *.liq) $(wildcard *.pl) \
	$(wildcard fixme/*.liq) \
	media/Makefile $(wildcard media/*.sh) $(wildcard media/*.liq media/*.liq.in media/*.json) \
	language/Makefile $(wildcard language/*.liq) \
	regression/Makefile $(wildcard regression/*.liq)

dist:
	mkdir $(DISTDIR)/tests
	rsync -amR $(DISTFILES) $(DISTDIR)/tests

.PHONY: test
