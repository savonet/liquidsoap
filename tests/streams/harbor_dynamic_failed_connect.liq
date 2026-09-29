%include "../test.liq"

# A client whose on_connect fails gets no source to feed it, so the input must
# close its connection itself.
def fn() =
  input.harbor.dynamic(
    port=9880,
    "/harbor_dynamic_failed_connect",
    on_connect=fun (_) -> error.raise(error.failure, "rejected")
  )

  o =
    output.icecast(
      %mp3,
      password="hackme",
      mount="harbor_dynamic_failed_connect",
      port=9880,
      noise()
    )

  o.on_error(synchronous=true, fun (~restart_in:_, _) -> test.pass())
  thread.run(
    delay=10.,
    {
      test.fail(
        "the rejected client was never disconnected"
      )
    }
  )
end

test.check(fn)
