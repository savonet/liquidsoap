DISTFILES = Makefile $(wildcard *.liq)

top_srcdir = ../../..
include $(top_srcdir)/Makefile.rules

test:
	@for i in $(wildcard *.liq) ; do \
	  echo -n "$$i... " ; $(top_srcdir)/src/liquidsoap -q $(top_srcdir)/scripts/pervasives.liq - < ./$$i || exit 1 ; \
	done
