DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)

top_srcdir = ../..
include $(top_srcdir)/Makefile.rules

	  # | tee /dev/null - | head -n 1 ; \

	  # if [ $$? -ne 0 ] ; then echo "TEST FAILED" ; echo exit 1 ; fi ; \

test: threesines
	@for i in $(wildcard *.liq) ; do \
	  echo -n "$$i... " ; $(top_srcdir)/src/liquidsoap -q ../pervasives.liq - < ./$$i || exit 1 ; \
	done
	@echo -n "type_errors.pl... " ; \
	  if (./type_errors.pl > /dev/null 2> /dev/null) ; then \
	    echo "TEST PASSED (check manually the prettiness of messages)" ; \
	  else \
	    echo "TEST FAILED" ; exit 1 ; \
	  fi

# Generate samples files in threesines directory

threesines: threesines/a.wav threesines/b.wav threesines/c.wav
MKSINE= ../../src/liquidsoap -q ../utils.liq \
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
