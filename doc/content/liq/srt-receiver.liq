s =
  input.srt(
    content_type=
      "application/ffmpeg;format=s16le,ch_layout=stereo,sample_rate=48000"
  )
output.dummy(fallible=true, s)
