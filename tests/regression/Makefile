LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))

test: $(TESTS)

%: %.liq
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" regression/$<

.PHONY: test
