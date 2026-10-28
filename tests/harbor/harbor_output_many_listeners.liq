%include "../test.liq"

# Twelve decoding clients, then stop: twelve connects and twelve disconnects,
# and `listeners()` is empty afterwards.
port = 18113
count = 12
o = output.harbor(port=port, mount="many.mp3", %mp3, sine())
connects = ref(0)
disconnects = ref(0)
o.on_connect(synchronous=true, fun (_) -> connects := connects() + 1)
o.on_disconnect(synchronous=true, fun (_) -> disconnects := disconnects() + 1)

def start_clients() =
  for _ = 1 to count do
    output.dummy(fallible=true, input.http("http://127.0.0.1:#{port}/many.mp3"))

  end
end

def check() =
  if
    connects() != count
  then
    test.fail(
      "#{connects()} connects"
    )
  end
  o.stop()
  thread.run(
    delay=2.,
    fun () ->
      begin
        test.equal(disconnects(), count)
        test.equal(o.listeners(), [])
        test.pass()
      end
  )
end

o.on_start(
  synchronous=true,
  fun () ->
    begin
      start_clients()
      thread.run(delay=5., check)
    end
)
