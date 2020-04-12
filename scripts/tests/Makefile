.PHONY: test flactest

DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)
LIQ = $(sort $(filter-out test.liq, $(wildcard *.liq)))
TESTS = $(basename $(LIQ))

top_srcdir = ../..

test: $(TESTS) flactest
	@./run_test.sh /usr/bin/perl ./type_errors.pl

%: %.liq
	@./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives -q ../../libs/pervasives.liq -" $<

flactest: flactest/a.flac
MKFLAC= ../../src/liquidsoap  --no-pervasives -q ../../libs/pervasives.liq \
          "clock.assign_new(sync='none',[ \
          output.file(%flac,'$$fname',fallible=true,on_stop=shutdown, \
          once(sine(duration=2.)))])"
flactest/a.flac: ../../src/liquidsoap Makefile
	mkdir -p flactest
	fname=$@ ; $(MKFLAC)

include $(top_srcdir)/Makefile.rules
