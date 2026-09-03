def f() =
  cover_png = file.cover("./cover.mp3")
  expected_png = file.contents("cover.png")
  test.equal("#{cover_png}", expected_png)

  # The extension must keep its leading dot for the cover file to be named
  # `<prefix>.png` rather than `<prefix>png`.
  def check_extension(~mime_types) =
    cover_file = file.cover.manager(mime_types=mime_types, default="cover.png")
    cover_file.set([("filename", "./cover.mp3")])
    cover_filename = request.uri(cover_file())
    test.equal(file.extension(cover_filename), ".png")
    test.equal(file.contents(cover_filename), expected_png)
  end

  check_extension(mime_types=null)
  check_extension(mime_types=[("image/png", ".png")])

  test.pass()
end

test.check(f)
