s = server.insert_metadata(sine(id="src"))

s.on_metadata(
  synchronous=true,
  fun (m) ->
    if
      m["title"] ==
        "New title"
      and m["artist"] == "Liquidsoap"
    then
      test.pass()
    end
)

output.dummy(s)

thread.run(
  delay=1.,
  {
    ignore(
      server.execute(
        'src.insert title="New title",artist="Liquidsoap"'
      )
    )
  }
)

thread.run(
  delay=5.,
  {
    test.fail(
      "metadata was not inserted"
    )
  }
)
