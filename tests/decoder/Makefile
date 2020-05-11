# https://github.com/stoyanovgeorge/ffmpeg/wiki/How-to-Compare-Video

all: test.mp4
	./convert2avi.liq test.mp4
	ffmpeg -i test.mp4 -i test.avi -filter_complex "[0:v]scale=1280x720:flags=bicubic[main]; [1:v]scale=1280x720:flags=bicubic,format=pix_fmts=yuv420p,fps=fps=25/1[ref]; [main][ref]ssim" -f null -

play:
	./play.liq test.mp4

test.mp4:
	ffmpeg -f lavfi -i "testsrc=size=1280x720:rate=30" -f lavfi -i "sine=frequency=440" -ac 2 -t 10 -pix_fmt yuv420p test.mp4
