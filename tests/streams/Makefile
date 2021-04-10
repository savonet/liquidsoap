LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(LIQ)
top_srcdir = $(shell realpath ../..)

test: files $(TESTS)

files:
	if [ ! -e file1.mp3 ] ; then ffmpeg -f lavfi -i "sine=frequency=220:duration=5" -ac 2 file1.mp3 ; fi
	if [ ! -e file2.mp3 ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 file2.mp3 ; fi
	if [ ! -e file3.mp3 ] ; then ffmpeg -f lavfi -i "sine=frequency=880:duration=5" -ac 2 file3.mp3 ; fi

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-pervasives $(top_srcdir)/libs/pervasives.liq -" streams/$<

include $(top_srcdir)/Makefile.rules

.PHONY: test files
