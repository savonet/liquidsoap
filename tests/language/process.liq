first = ref(true)
thread.run.recurrent(
  {
    if
      first()
    then
      begin
        first := false
        2.
      end
    else
      begin
        test.equals(
          process.read(
            "printf toto"
          ),
          "toto"
        )

        p =
          process.run(
            "exit 2"
          )
        test.equals(p.status, "exit")
        test.equals(p.status.code, 2)

        p =
          process.run(
            timeout=0.5,
            "sleep 1"
          )
        test.equals(p.status, "timeout")
        test.equals(
          process.quote.command(
            stdin="bla",
            stdout="blo",
            stderr="blo",
            "foo",
            args=["gni", 'gno"gna']
          ),
          "'foo' 'gni' 'gno\"gna' <'bla' >'blo' 2>&1"
        )

        test.pass()
        (-1.)
      end
    end
  }
)

clock.autostart := false
output.dummy(blank())
