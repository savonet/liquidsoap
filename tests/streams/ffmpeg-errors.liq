def f() =
  def on_error(e) =
    e = error.methods(e)
    test.equal(e.kind, "ffmpeg")
    test.equal(
      e.message,
      "Avutil.Error(No such file or directory)"
    )
    let [..._, {filename}] = e.trace
    test.equal(filename, "src/core/io/ffmpeg_io.ml")
    test.pass()
  end

  s = input.ffmpeg("invalid")
  s.on_error(on_error)
  output.dummy(s, fallible=true)
end

test.check(f)
