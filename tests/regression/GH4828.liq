%include "../test.liq"

log.level := 4

# The audio of a crossed sequence of files, compared sample for sample: it is
# written to a file whose digest is checked. The same digest is obtained with
# any frame size (`settings.frame.duration`): nothing in a crossing depends on
# where frames are cut.
expected_digest = "2b43f0eeb418eb76265d61208248e7bc"

audio_file = "#{test.assets_dir()}/GH4828.flac"
out = file.temp(cleanup=true, "GH4828", ".wav")

s = single("annotate:liq_cue_in=0.0,liq_cue_out=16.7:#{audio_file}")

s = sequence([s, s, fallback([])])

def transition({source = old}, {source = new}) =
  sequence([old, new])
end

s = cross(deduplicate=false, duration=1.1, transition, s)

clock.assign_new(sync='none', [s])
o = output.file(fallible=true, %wav, out, s)

def on_stop() =
  test.equal(file.digest(out), expected_digest)
  test.pass()
end

o.on_stop(synchronous=true, on_stop)
