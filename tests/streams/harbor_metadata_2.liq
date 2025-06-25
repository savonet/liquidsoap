port = 3464

def fn() =
  def on_metadata(m) =
    if

        m["title"] ==
          "the real title"

    and

        m["metadata_url"] ==
          "metadata url"

    then
      test.pass()
    end
  end

  def on_connect(_) =
    thread.run(
      delay=1.,
      {
        icy.update_metadata(
          password="testtest",
          user="testtest",
          host="localhost",
          port=port,
          mount="test",
          [
            (
              "song",
              "song title"
            ),
            (
              "title",
              "the real title"
            ),
            (
              "url",
              "metadata url"
            )
          ]
        )
      }
    )
  end

  s =
    input.harbor(
      buffer=0.1,
      password="testtest",
      user="testtest",
      "test",
      port=port,
      on_connect=on_connect
    )

  s.on_metadata(synchronous=true, on_metadata)
  output.dummy(fallible=true, s)
  output.icecast(
    %mp3, password="testtest", user="testtest", mount="test", port=port, noise()
  )
end

test.check(fn)
