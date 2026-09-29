%include "../test.liq"

# A client rejected for its codec must not keep the mount from the next one.
def fn() =
  s = input.harbor(buffer=0.1, port=9882, "harbor_unknown_codec")

  s.on_connect(synchronous=true, fun (_) -> test.pass())

  output.dummy(fallible=true, s)
  output.icecast(
    %mp3,
    format="application/x-unknown-codec",
    send_icy_metadata=false,
    password="hackme",
    mount="harbor_unknown_codec",
    port=9882,
    noise()
  )

  thread.run(
    delay=2.,
    {
      ignore(
        output.icecast(
          %mp3,
          password="hackme",
          mount="harbor_unknown_codec",
          port=9882,
          noise()
        )
      )
    }
  )

  thread.run(
    delay=10.,
    {
      test.fail(
        "the mount stayed taken after a rejected client"
      )
    }
  )
end

test.check(fn)
