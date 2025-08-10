s =
  playlist(
    cue_in_metadata=null,
    cue_out_metadata=null,
    prefix="annotate:liq_cue_in=1.,liq_cue_out=3.:",
    "playlist"
  )

is_first = ref(true)

def on_frame() =
  if
    is_first()
  then
    if 5. <= source.duration(s) then test.pass() else test.fail() end
  end
  is_first := false
end

s.on_frame(synchronous=true, on_frame)

output.dummy(fallible=true, s)
