s =
  input.srt(
    content_type="application/ffmpeg;format=s16le,ch_layout=stereo,sample_rate=48000"
  )

# END
output.dummy(fallible=true, s)
