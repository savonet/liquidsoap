
DISTFILES = $(wildcard *.in) $(wildcard *.liq) $(wildcard *.conf) \
			Makefile README

top_srcdir = ..
include $(top_srcdir)/Makefile.rules

.PHONY: doc-install
doc-install:
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/examples
	$(INSTALL_DATA) $(wildcard *.liq) README $(datadir)/doc/$(DISTDIR)/examples
