# Listeners keep reconnecting while the output is repeatedly stopped and
# started: every connect must be matched by exactly one disconnect, and no
# listener may be left behind.
def f() =
  settings.harbor.output.writers := 3
  connected = ref(0)
  disconnected = ref(0)

  o = output.harbor(%mp3, mount="test", port=3478, sine())

  def bump(counter) =
    atomic(fast=true, fun () -> counter := counter() + 1)
  end

  o.on_connect(synchronous=true, fun (_) -> bump(connected))
  o.on_disconnect(synchronous=true, fun (_) -> bump(disconnected))

  for _ = 1 to 8 do
    l = input.http(max_buffer=1., "http://localhost:3478/test")
    output.dummy(fallible=true, l)

  end

  cycles = ref(0)

  def rec toggle() =
    cycles := cycles() + 1
    if
      cycles() <= 10
    then
      o.stop()

      # Harbor closes the listening socket asynchronously: an immediate
      # restart can fail to bind the port.
      thread.run(
        delay=0.2,
        fun () ->
          begin
            o.start()
            thread.run(delay=0.5, toggle)
          end
      )
    else
      o.stop()
      thread.run(
        delay=3.,
        fun () ->
          begin
            test.equal(connected() > 8, true)
            test.equal(disconnected(), connected())
            test.equal(o.listeners(), [])
            test.pass()
          end
      )
    end
  end

  thread.run(delay=2., toggle)
end

test.check(f)
