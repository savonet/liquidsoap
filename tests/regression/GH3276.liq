log.level := 5
port = 8125

thread.run(
  delay=7.,
  fun () ->
    begin
      s = input.http("http://localhost:#{port}/test")
      track_count = ref(0)
      s.on_track(
        fast=true,
        fun (_) ->
          begin
            if track_count() > 1 then test.pass() end
            track_count := track_count() + 1
          end
      )

      output.dummy(fallible=true, s)
    end
)

output.harbor(%vorbis, chop(every=5., sine()), port=port, mount="/test")
