#!/bin/bash
# Fixtures of the known-complexity checks (tests.md §2.1), generated with the
# FFmpeg command line into the directory given as argument.
set -e
cd "$1"
enc=(-c:v libx264 -preset ultrafast -bf 2 -g 30 -pix_fmt yuv420p)
gen() { [ -f "$1" ] || ffmpeg -nostdin -loglevel error "${@:2}" "$1"; }
# B-frame concat set: video only, first DTS about 41.17, 25.2 and 8.17 s.
gen bf1.mp4 -f lavfi -i testsrc=duration=20:size=320x180:rate=30 "${enc[@]}" -output_ts_offset 41.25
gen bf2.mp4 -f lavfi -i testsrc=duration=30:size=320x180:rate=30 "${enc[@]}" -output_ts_offset 25.267
gen bf3.mp4 -f lavfi -i testsrc=duration=10:size=320x180:rate=30 "${enc[@]}" -output_ts_offset 8.25
# Mid-GOP pair: AAC + H.264, a keyframe every second; the second file starts
# 0.6 s before a keyframe.
gen gop.mp4 -f lavfi -i testsrc=duration=6:size=320x180:rate=30 -f lavfi -i sine=frequency=440:duration=6 "${enc[@]}" -c:a aac
gen midgop.mp4 -i gop.mp4 -ss 0.4 -c copy -copyinkf
# A/V offset set: audio ends 200 ms before video.
gen avoff.mp4 -f lavfi -i testsrc=duration=3:size=320x180:rate=30 -f lavfi -i sine=frequency=440:duration=2.8 "${enc[@]}" -c:a aac
# A source whose video ends before its first keyframe.
gen nokey.mp4 -i midgop.mp4 -t 0.4 -c copy -copyinkf
# A video whose first keyframe comes 25 s in, beyond the keyframe wait bound.
gen longgop_full.mp4 -f lavfi -i testsrc=duration=60:size=160x90:rate=30 -f lavfi -i sine=frequency=440:duration=60 -c:v libx264 -preset ultrafast -bf 2 -g 900 -keyint_min 900 -sc_threshold 0 -pix_fmt yuv420p -c:a aac
gen latekey.mp4 -i longgop_full.mp4 -ss 5 -t 30 -c copy -copyinkf
# MPEG-TS with a declared timestamp discontinuity whose bitstream continues:
# two segments of one encode, byte-concatenated, the second cut mid-GOP and
# its timestamps restarting.
gen tsa.ts -i gop.mp4 -t 3.4 -c copy -bsf:v h264_mp4toannexb
gen tsb.ts -i gop.mp4 -ss 3.4 -c copy -copyinkf -bsf:v h264_mp4toannexb -output_ts_offset 0
[ -f discont.ts ] || cat tsa.ts tsb.ts > discont.ts
