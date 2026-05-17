source = sine()

# BEGIN
# AAC audio in an MPEG-TS container
output.file(%ffmpeg(format="mpegts", %audio(codec="aac", b="128k")), "/tmp/out.ts", source)
# END
