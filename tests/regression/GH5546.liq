%include "../test.liq"

# Regression for https://github.com/savonet/liquidsoap/issues/5546: a late
# listener's burst spans several Ogg links and must start at the current one.
settings.decoder.decoders := ["ffmpeg"]

port = 3546
s = insert_metadata(sine())
links = ref(0)

thread.run(
  every=1.,
  fun () ->
    begin
      ref.incr(links)
      s.insert_metadata(
        [
          (
            "title",
            "link #{links()}"
          )
        ]
      )
    end
)

output.harbor(port=port, mount="test", %ogg(%opus), s)

thread.run(
  delay=12.,
  fun () ->
    begin
      frames = ref(0)
      listener = input.http("http://127.0.0.1:#{port}/test")
      listener.on_frame(synchronous=true, fun () -> ref.incr(frames))
      listener.on_error(synchronous=true, fun (_) -> test.fail())
      def check() =
        if frames() > 50 then test.pass() else test.fail() end
      end

      listener.on_connect(
        synchronous=true,
        fun (_) -> thread.run(delay=3., check)
      )
      output.dummy(fallible=true, listener)
    end
)
