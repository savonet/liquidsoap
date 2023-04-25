def f() =
  output.shoutcast(
    %mp3,
    host="shoutcast.example.org",
    port=8000,
    password="changeme",
    sine()
  )

  test.pass()
end

test.check(f)
