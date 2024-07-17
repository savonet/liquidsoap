
DISTFILES = liquidsoap.1 Makefile
SUBDIRS = html wiki

top_srcdir=..
include $(top_srcdir)/Makefile.rules

.PHONY: doc-install
doc-install:
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/css
	for doc in $(wildcard html/*.html) ; do \
		$(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR) ; \
	done
	for doc in $(wildcard html/css/*.css) ; do \
		$(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/css ; \
	done
	make -C wiki doc-install
