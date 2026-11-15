%include "../test.liq"

# source.drop.* hides a track on the tracks value only: the muxed source must
# not get it back from the source it was taken from.
s = (blank() : source(audio=pcm(stereo), video=yuv420p))
audio = source.drop.video(s)
video = source.drop.audio(s)
output.dummy(fallible=true, s)
output.dummy(fallible=true, audio)
output.dummy(fallible=true, video)

def check() =
  test.equal(list.map(fst, source.content(audio)), ["audio"])
  test.equal(list.map(fst, source.content(video)), ["video"])
  test.pass()
end

test.check(check)
