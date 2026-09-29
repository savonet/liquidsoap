%include "../test.liq"

# on_connect runs while harbor still has to write the reply on the client's
# socket, so stopping the input there must not free that socket under harbor.
def fn() =
  s = input.harbor(buffer=0.1, port=9881, "harbor_stop_during_relay")

  stops = ref(0)

  s.on_connect(
    synchronous=true,
    fun (_) ->
      begin
        stops := stops() + 1
        s.stop()
        if stops() == 2 then test.pass() end
      end
  )

  output.dummy(fallible=true, s)
  output.icecast(
    %mp3,
    password="hackme",
    mount="harbor_stop_during_relay",
    port=9881,
    noise()
  )

  thread.run(
    delay=10.,
    {
      test.fail(
        "the input did not take a client after a stop"
      )
    }
  )
end

test.check(fn)
