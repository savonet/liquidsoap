s = single("annotate:liq_cue_in=3.,liq_cue_out=15.:../media/@wav[stereo].wav")

track_count = ref(0)

def on_track(_) =
  track_count := track_count() + 1
  if track_count() > 3 then
    shutdown()
  end
end

s = source.on_track(s, on_track)

s = amplify(id="amp", 1.0, s)

s = bpm(s)

s = cue_cut(s)

s = accelerate(ratio=0.8, s)

s = crossfade(smart=true,s)

clock.assign_new(sync="none",[s])

output.dummy(fallible=true,s)
