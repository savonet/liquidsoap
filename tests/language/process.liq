test.skip()

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
        test.equal(
          process.read(
            "printf toto"
          ),
          "toto"
        )

        p =
          process.run(
            "exit 2"
          )
        test.equal(p.status, "exit")
        test.equal(p.status.code, 2)

        p =
          process.run(
            timeout=0.5,
            "sleep 1"
          )
        test.equal(p.status, "timeout")
        test.equal(
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

output.dummy(blank())
