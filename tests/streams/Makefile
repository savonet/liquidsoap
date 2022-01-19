.PHONY: test

LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(LIQ)
top_srcdir = $(shell realpath ../..)

include $(top_srcdir)/Makefile.rules

test: files $(TESTS)

files:
	mkdir -p files
	ffmpeg -f lavfi -i "sine=frequency=220:duration=5" -ac 2 files/file1.mp3
	ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 files/file2.mp3
	ffmpeg -f lavfi -i "sine=frequency=880:duration=5" -ac 2 files/file3.mp3
	ffmpeg -f lavfi -i "sine=frequency=220:duration=2" -ac 2 files/jingle1.mp3
	ffmpeg -f lavfi -i "sine=frequency=440:duration=2" -ac 2 files/jingle2.mp3
	ffmpeg -f lavfi -i "sine=frequency=880:duration=2" -ac 2 files/jingle3.mp3
	echo "jingle1.mp3\njingle2.mp3\njingle3.mp3" > files/jingles
	echo "file1.mp3\nfile2.mp3\nfile3.mp3" > files/playlist
	for i in `seq 1 100000`; do echo "file$$i.mp3" >> files/huge_playlist; done

clean:
	rm -rf files

%: %.liq
	@../run_test.sh "$(top_srcdir)/src/liquidsoap --no-stdlib $(top_srcdir)/libs/stdlib.liq -" streams/$<
