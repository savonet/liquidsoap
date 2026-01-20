s = sine()
port = 3467

o = output.harbor(port=port, mount="test", %mp3, s)

o.on_stop(synchronous=true, test.pass)

thread.run(
  delay=1.,
  fun () ->
    begin
      s = input.http("http://127.0.0.1:#{port}/test")
      output.dummy(fallible=true, s)

      s.on_error(synchronous=true, fun (_) -> test.fail())
      s.on_connect(
        synchronous=true,
        fun (_) -> begin thread.run(delay=2., o.stop) end
      )
    end
)
