%include "../test.liq"

# ID3v2 rendering: a date is TYER in v2.3 and TDRC in v2.4 (TDAT holds a day
# and month), genre and albumartist map, and a key given twice gives one
# frame, the last one.

def count(substring, s) =
  list.length(string.split(separator=substring, s)) - 1
end

def f() =
  m =
    [
      ("title", "first"),
      ("date", "2024"),
      ("genre", "rock"),
      ("albumartist", "band"),
      ("title", "last")
    ]

  v3 = metadata.id3v2.render(version=3, m)
  v4 = metadata.id3v2.render(version=4, m)
  test.equal(count("TYER", v3), 1)
  test.equal(count("TDAT", v3), 0)
  test.equal(count("TDRC", v4), 1)
  test.equal(count("TCON", v4), 1)
  test.equal(count("TPE2", v4), 1)
  test.equal(count("TIT2", v4), 1)
  test.equal(string.contains(encoding="ascii", substring="last", v4), true)
  test.equal(string.contains(encoding="ascii", substring="first", v4), false)
  test.pass()
end

test.check(f)
