LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))

test: $(TESTS) type_errors

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" language/$<

type_errors: type_errors.pl
	@../run_test.sh /usr/bin/perl language/$<

.PHONY: test type_errors
