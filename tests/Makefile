.PHONY: test flactest

DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)
LIQ = $(sort $(filter-out test.liq, $(wildcard *.liq)))
TESTS = $(basename $(LIQ))

top_srcdir = ..

test: flactest $(TESTS)
	@./run_test.sh /usr/bin/perl ./type_errors.pl
	@echo
	@echo
	@echo "***** Performance tests *****"
	@./performance.liq

perf:
	perf record --call-graph=dwarf -- ../src/liquidsoap ../libs/pervasives.liq performance.liq
	perf report

%: %.liq
	@./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives -q $(top_srcdir)/libs/pervasives.liq -" $<

flactest: flactest/a.flac
MKFLAC=$(top_srcdir)/src/liquidsoap  --no-pervasives -q $(top_srcdir)/libs/pervasives.liq \
          "clock.assign_new(sync='none',[ \
          output.file(%flac,'$$fname',fallible=true,on_stop=shutdown, \
          once(sine(duration=2.)))])"
flactest/a.flac: $(top_srcdir)/src/liquidsoap Makefile
	mkdir -p flactest
	fname=$@ ; $(MKFLAC)

include $(top_srcdir)/Makefile.rules
