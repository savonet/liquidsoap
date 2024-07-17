
DISTFILES = soaptube.1 liquidsoap.1 Makefile
SUBDIRS = html

top_srcdir=..
include $(top_srcdir)/Makefile.rules

.PHONY: doc-install
doc-install:
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1
	$(INSTALL_DATA) soaptube.1 $(mandir)/man1
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/html/css
	for doc in $(wildcard html/*.html) ; do \
		$(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/html ; \
	done
	for doc in $(wildcard html/css/*.css) ; do \
		$(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/html/css ; \
	done
