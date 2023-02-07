LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))

test: $(TESTS)

files:
	mkdir -p files
	ffmpeg -f lavfi -i "sine=frequency=220:duration=5" -ac 2 files/file1.mp3
	ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 files/file2.mp3
	ffmpeg -f lavfi -i "sine=frequency=880:duration=5" -ac 2 files/file3.mp3
	ffmpeg -f lavfi -i "sine=frequency=220:duration=2" -ac 2 files/jingle1.mp3
	ffmpeg -f lavfi -i "sine=frequency=440:duration=2" -ac 2 files/jingle2.mp3
	ffmpeg -f lavfi -i "sine=frequency=880:duration=2" -ac 2 files/jingle3.mp3
	ffmpeg -f lavfi -i color=size=320x240:color=blue -vf "drawtext=fontsize=30:fontcolor=white:x=(w-text_w)/2:y=(h-text_h)/2:text='Test 1'" -frames:v 1 files/file1.png
	ffmpeg -f lavfi -i color=size=500x100:color=red -vf "drawtext=fontsize=30:fontcolor=white:x=(w-text_w)/2:y=(h-text_h)/2:text='Test 2'" -frames:v 1 files/file2.png
	echo "jingle1.mp3\njingle2.mp3\njingle3.mp3" > files/jingles
	echo "file1.mp3\nfile2.mp3\nfile3.mp3" > files/playlist
	for i in `seq 1 100000`; do echo "file$$i.mp3" >> files/huge_playlist; done

clean:
	rm -rf files

%: %.liq files
	@../run_test.sh "../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq -" streams/$<

.PHONY: test
