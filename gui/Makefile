
PY=$(wildcard *.py)

DISTFILES = Makefile $(wildcard *.py)
top_srcdir=..
include $(top_srcdir)/Makefile.rules

ifeq ($(W_PYTHON),yes)
install-local:
	for p in $(PY) ; do \
	  $(INSTALL_PROGRAM) -D $$p $(PYMDIR)/$$p ; \
	done
	$(INSTALL_PROGRAM) -D liguidsoap $(bindir)/liguidsoap
endif
