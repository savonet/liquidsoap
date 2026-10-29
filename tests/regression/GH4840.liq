%include "../test.liq"

# Stopping an output.harbor with a decoding listener connected fires the stop
# callback and raises no error before it.
port = 18120
o = output.harbor(port=port, mount="gh4840.mp3", %mp3, sine())
connected = ref(false)
o.on_connect(synchronous=true, fun (_) -> connected := true)
o.on_stop(
  synchronous=true,
  fun () ->
    begin
      test.equal(connected(), true)
      test.pass()
    end
)

o.on_start(
  synchronous=true,
  fun () ->
    begin
      output.dummy(
        fallible=true,
        input.http("http://127.0.0.1:#{port}/gh4840.mp3")
      )
      thread.run(delay=3., fun () -> o.stop())
    end
)
