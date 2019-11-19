DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)
LIQ = $(sort $(filter-out test.liq, $(wildcard *.liq)))
TESTS = $(basename $(LIQ))

top_srcdir = ../..

test: $(TESTS)
	@./run_test.sh /usr/bin/perl ./type_errors.pl

%: %.liq
	@./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives -q ../../libs/pervasives.liq -" $<

include $(top_srcdir)/Makefile.rules
