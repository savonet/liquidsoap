%include "../test.liq"

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
  s =
    input.ffmpeg(
      " ./file1.mp3\n"
    )
  s.on_connect(synchronous=true, on_connect)
  s.on_error(synchronous=true, on_error)
  output.dummy(s, fallible=true)
end

test.check(f)
