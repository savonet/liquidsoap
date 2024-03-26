def f() =
  cover_png = file.cover("./cover.mp3")
  expected_png = file.contents("cover.png")
  test.equal("#{cover_png}", expected_png)
  test.pass()
end

test.check(f)
