%include "../test.liq"

s =
  playlist(
    cue_in_metadata=null,
    cue_out_metadata=null,
    prefix="annotate:liq_cue_in=1.,liq_cue_out=3.:",
    "playlist"
  )

# The request is taken when the source is first read: its duration is known
# from the second frame on.
frames = ref(0)

def on_frame() =
  ref.incr(frames)
  if
    frames() == 2
  then
    if 5. <= source.duration(s) then test.pass() else test.fail() end
  end
end

s.on_frame(synchronous=true, on_frame)

output.dummy(fallible=true, s)
