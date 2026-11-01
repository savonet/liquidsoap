%include "../test.liq"

# A client whose on_connect fails gets no source to feed it, so the input must
# close its connection itself. A client whose source is handed to on_connect
# but never started gets its on_disconnect callbacks once its deadline passes.
def fn() =
  input.harbor.dynamic(
    port=9880,
    "/harbor_dynamic_failed_connect",
    on_connect=fun (_) -> error.raise(error.failure, "rejected")
  )

  disconnects = ref(0)
  input.harbor.dynamic(
    port=9880,
    timeout=1.,
    "/harbor_dynamic_deadline",
    on_connect=fun (_) ->
      fun (s) ->
        s.on_disconnect(
          synchronous=true,
          fun () -> atomic(fun () -> disconnects := disconnects() + 1)
        )
  )

  rejected =
    output.icecast(
      %mp3,
      password="hackme",
      mount="harbor_dynamic_failed_connect",
      port=9880,
      noise()
    )

  unused =
    output.icecast(
      %mp3,
      password="hackme",
      mount="harbor_dynamic_deadline",
      port=9880,
      noise()
    )

  rejected_closed = ref(false)
  rejected.on_error(
    synchronous=true,
    fun (~restart_in:_, _) -> rejected_closed := true
  )
  thread.run(
    every=0.1,
    {
      if
        rejected_closed() and disconnects() > 0
      then
        unused.shutdown()
        if
          disconnects() == 1
        then
          test.pass()
        else
          test.fail(
            "on_disconnect ran #{disconnects()} times"
          )
        end
      end
    }
  )
  thread.run(
    delay=10.,
    {
      if
        not rejected_closed()
      then
        test.fail(
          "the rejected client was never disconnected"
        )
      else
        test.fail(
          "on_disconnect never ran for the client past its deadline"
        )
      end
    }
  )
end

test.check(fn)
