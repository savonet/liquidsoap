log.level := 4

expected_checksum = "7f3bf58be51d495b6fee66af8a89c784"

settings.frame.checksum.metadata := true
settings.frame.checksum.track_marks := true

sha_file = file.temp(cleanup=true, "cross-crack-sha", ".txt")
audio_file = "#{test.assets_dir()}/GH4828.flac"

s = single("annotate:liq_cue_in=0.0,liq_cue_out=16.7:#{audio_file}")

s = sequence([s, s, fallback([])])

def transition({source = old}, {source = new}) =
  sequence([old, new])
end

s = cross(deduplicate=false, duration=1.1, transition, s)

variable_meta = ["filename", "initial_uri"]
s =
  metadata.map(
    update=false,
    fun (m) ->
      list.filter(fun ((lbl, _)) -> not list.mem(lbl, variable_meta), m),
    s
  )

s.on_frame_checksum(
  synchronous=true,
  fun (~cache:_, checksum) ->
    file.write(append=true, data="#{checksum}\n", sha_file)
)

clock.assign_new(sync='none', [s])
o = output.dummy(fallible=true, s)

def on_stop() =
  d = file.digest(sha_file)
  test.equal(d, expected_checksum)
  test.pass()
end

o.on_stop(synchronous=true, on_stop)
