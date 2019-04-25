DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)

top_srcdir = ../..
include $(top_srcdir)/Makefile.rules

test: threesines
	@for i in $(wildcard *.liq) ; do \
	  ./run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives -q ../../lib/pervasives.liq -" $$i ; \
	done
	@./run_test.sh /usr/bin/perl ./type_errors.pl

# Generate samples files in threesines directory

threesines: threesines/a.wav threesines/b.wav threesines/c.wav
MKSINE= ../../src/liquidsoap  --no-pervasives -q ../../lib/utils.liq \
	  "clock.assign_new(sync=false,[ \
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
