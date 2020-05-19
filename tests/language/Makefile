.PHONY: test

LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(LIQ)
top_srcdir = $(shell realpath ../..)

test: $(TESTS)

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" language/$<

include $(top_srcdir)/Makefile.rules
