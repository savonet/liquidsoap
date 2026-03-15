def f() =
  is_done = ref(false)

  def on_error(_) =
    if not is_done() then test.fail() end
    is_done := true
  end

  def on_connect() =
    if not is_done() then test.pass() end
    is_done := true
  end

  # Test string.trim on url input.
  output.dummy(
    input.ffmpeg(
      on_connect=on_connect,
      on_error=on_error,
      " ./file1.mp3\n"
    ),
    fallible=true
  )
end

test.check(f)
