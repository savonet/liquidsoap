port = 9502
settings.srt.prefer_address := "ipv4"

track_count = ref(0)

def on_track(_) =
  ref.incr(track_count)
  if track_count() > 1 then test.pass() end
end

s = input.srt(mode="caller", content_type="application/ogg", port=port)

s.on_track(on_track)

output.dummy(fallible=true, s)

output.srt(id="output.srt", mode="listener", port=port, %opus, sine())

thread.run(delay=3., s.disconnect)

thread.run(delay=4., s.connect)
