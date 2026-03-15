s = playlist(prefix="annotate:liq_cue_in=1.,liq_cue_out=3.:", "playlist")

is_first = ref(true)

def on_frame() =
  if
    is_first()
  then
    if source.duration(s) == 2. then test.pass() else test.fail() end
  end
  is_first := false
end

s.on_frame(synchronous=true, on_frame)

output.dummy(fallible=true, s)
