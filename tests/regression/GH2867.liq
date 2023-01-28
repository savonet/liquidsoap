def f() =
  output.file(
    %mp3(stereo=false),
    "/tmp/bla",
    mean(blank()))

  test.pass()
end

test.check(f)
