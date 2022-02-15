LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))

test: $(TESTS) type_errors

%: %.liq
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" language/$<

type_errors: type_errors.pl
	@../run_test.sh /usr/bin/perl language/$<

.PHONY: test type_errors
