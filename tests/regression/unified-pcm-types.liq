# Type parameters below are shared between pcm_s16 source and the decoded
# source. We want to make sure that this works at runtime.
def f() =
  s = blank()

  s.on_track(synchronous=true, fun (_) -> test.pass())

  s = audio.decode.pcm_s16(s)

  output.file(%ffmpeg(%audio(codec = "aac")), "unified-pcm-types.aac", s)
end

test.check(f)
