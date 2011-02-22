
DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)

top_srcdir = ../..
include $(top_srcdir)/Makefile.rules

test:
	@for i in $(wildcard *.liq) ; do \
	  echo -n "$$i... " ; $(top_srcdir)/src/liquidsoap -q - < ./$$i | head -n 1 ; \
	done
	@echo -n "type_errors.pl... " ; \
	  if (./type_errors.pl > /dev/null 2> /dev/null) ; then \
	    echo "TEST PASSED (check manually the prettiness of messages)" ; \
      else \
	    echo "TEST FAILED" ; \
	  fi
