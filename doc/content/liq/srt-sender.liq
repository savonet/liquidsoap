s = sine()
enc = %ffmpeg(format = "s16le", %audio(codec = "pcm_s16le", ac = 2, ar = 48000))
output.srt(enc, s)
