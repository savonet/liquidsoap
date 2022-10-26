.PHONY: test server_port

LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))

test: server_port $(TESTS)

%: %.liq
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" regression/$<

server_port:
	@../run_test.sh ./server_port.sh regression/server_port.sh
