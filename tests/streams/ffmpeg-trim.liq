def f() =
  def on_error(_) =
    test.fail()
  end

  # Test string.trim on url input.
  output.dummy(
    input.ffmpeg(on_connect=test.pass, on_error=on_error, " ./file1.mp3\n"),
    fallible=true
  )
end

test.check(f)
