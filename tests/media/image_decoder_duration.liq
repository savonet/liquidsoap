s = single("annotate:duration=0.2:test-image.png")

track_count = ref(0)

s.on_track(
  fun (_) ->
    begin
      ref.incr(track_count)
      if track_count() > 2 then test.pass() end
    end
)

s = (s : source(video=canvas))

output.dummy(s)
