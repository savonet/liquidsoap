DISTFILES = Makefile $(wildcard *.liq) $(wildcard *.pl)

top_srcdir = ../..
include $(top_srcdir)/Makefile.rules

test: threesines
	@for i in $(wildcard *.liq) ; do \
	  echo -n "$$i... " ; $(top_srcdir)/src/liquidsoap -q ../pervasives.liq - < ./$$i | \
		tee /dev/null - | head -n 1 ; \
	done
	@echo -n "type_errors.pl... " ; \
	  if (./type_errors.pl > /dev/null 2> /dev/null) ; then \
	    echo "TEST PASSED (check manually the prettiness of messages)" ; \
      else \
	    echo "TEST FAILED" ; \
	  fi

# Generate samples files in threesines directory

threesines: threesines/a.wav threesines/b.wav threesines/c.wav
MKSINE= ../../src/liquidsoap ../utils.liq \
	  "clock.assign_new(sync=false,[ \
	  output.file(%wav,\"$$fname\",fallible=true,on_stop=shutdown, \
	  once(sine(duration=2.,$$freq)))])"
threesines/a.wav: threesines ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=220. ; $(MKSINE)
threesines/b.wav: threesines ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=330. ; $(MKSINE)
threesines/c.wav: threesines ../../src/liquidsoap Makefile
	mkdir -p threesines
	fname=$@ ; freq=440. ; $(MKSINE)
