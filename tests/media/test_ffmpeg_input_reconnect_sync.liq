%include "../test.liq"

# Each connection ends with video past its audio. The track ends where both
# fields line up, so the skew seen when a connection starts never grows.
fname = argv(default="", 1)

s = input.ffmpeg(self_sync=false, fname)

tracks = ref(0)
first_skew = ref(null)

def check(_) =
  buffered = s.buffered()
  skew = list.assoc("video", buffered) - list.assoc("audio", buffered)
  first_skew := first_skew() ?? skew
  drift = abs(skew - null.get(first_skew()))
  tracks := tracks() + 1
  print(
    "Track #{tracks()}: skew #{skew}s, drift #{drift}s"
  )
  if drift > 0.1 then test.fail() elsif tracks() >= 4 then test.pass() end
end

s.on_track(synchronous=true, check)

output.dummy(
  fallible=true,
  (s : source(audio=ffmpeg.audio.raw, video=ffmpeg.video.raw))
)
