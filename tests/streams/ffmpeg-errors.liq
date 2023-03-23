def f() =
  def on_error(e) =
    test.equals(e.kind, "ffmpeg")
    test.equals(e.message, "Avutil.Error(No such file or directory)")
    let [..., {position_start = {filename}}] = e.trace
    test.equals(filename, "src/core/io/ffmpeg_io.ml")
    test.pass()
  end

  output.dummy(input.ffmpeg(on_error=on_error, "invalid"), fallible=true)
end

test.check(f)
