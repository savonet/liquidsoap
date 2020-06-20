.PHONY: test type_errors

LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(LIQ)
top_srcdir = $(shell realpath ../..)

test: type_errors $(TESTS)

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" language/$<

type_errors: type_errors.pl
	@../run_test.sh /usr/bin/perl language/$<

include $(top_srcdir)/Makefile.rules
