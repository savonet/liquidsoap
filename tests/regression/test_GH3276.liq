log.level := 5
port = 8125

test.skip()

s = source.dynamic(infallible=true, track_sensitive=true, {sine(duration=2.)})

s = metadata.map(insert_missing=true, fun (_) -> [("title", "gno")], s)

output.harbor(%vorbis, s, port=port, mount="/test")

s = input.http("http://127.0.0.1:#{port}/test")

track_count = ref(0)
s.on_track(
  synchronous=true,
  fun (_) ->
    begin
      if track_count() > 1 then test.pass() end
      ref.incr(track_count)
    end
)

output.dummy(fallible=true, s)
