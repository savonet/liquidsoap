PY=$(wildcard *.py)

DISTFILES = Makefile $(PY) liguidsoap.1
top_srcdir=..
include $(top_srcdir)/Makefile.rules

ifeq ($(W_PYTHON),yes)
install-local:
	for p in $(PY) ; do \
	  $(INSTALL_PROGRAM) -D $$p $(PYMDIR)/$$p ; \
	done
	$(INSTALL_PROGRAM) -D liguidsoap $(bindir)/liguidsoap
	$(INSTALL_PROGRAM) -D liguidsoap.1 $(mandir)/man1/liguidsoap.1
endif
