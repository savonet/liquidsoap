DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)
LIQ = $(sort $(filter-out test.liq, $(wildcard *.liq)))
TESTS = $(basename $(LIQ))

top_srcdir = ../..

test: threesines $(TESTS)
	@./run_test.sh /usr/bin/perl ./type_errors.pl

%: %.liq
	@./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives -q ../../libs/pervasives.liq -" $<

# Generate samples files in threesines directory

threesines: threesines/a.wav threesines/b.wav threesines/c.wav
MKSINE= ../../src/liquidsoap  --no-pervasives -q \
	  ../../libs/list.liq ../../libs/utils.liq \
	  "clock.assign_new(sync=\"none\",[ \
	  output.file(%wav,\"$$fname\",fallible=true,on_stop=shutdown, \
	  once(sine(duration=2.,$$freq)))])"
threesines/a.wav: ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=220. ; $(MKSINE)
threesines/b.wav: ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=330. ; $(MKSINE)
threesines/c.wav: ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=440. ; $(MKSINE)

include $(top_srcdir)/Makefile.rules
