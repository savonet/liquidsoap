set("log.level",4)

def f() =
  m = file.metadata.taglib("@mp3[mono].mp3")

  if m["track"] != "" then
    test.fail()
  end

  if m["tracknumber"] == "0" then
    test.fail()
  end

  if m["year"] == "0" then
    test.fail()
  end

  test.pass()
end

test.check(f)
