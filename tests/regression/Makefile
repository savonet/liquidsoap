.PHONY: test server_port

LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(LIQ)
top_srcdir = $(shell realpath ../..)

test: server_port $(TESTS) 

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" regression/$<

server_port:
	@../run_test.sh $(top_srcdir)/tests/regression/server_port.sh regression/server_port.sh
	

include $(top_srcdir)/Makefile.rules
