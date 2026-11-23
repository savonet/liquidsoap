%include "../test.liq"

# An invalid UTF-8 sequence used to swallow the bytes after it, as many as its
# first byte announced: "Coraz\xf3n Serrano" became "Coraz�errano" (#5284).
# Only the invalid bytes are replaced now.

def f() =
  test.equal(
    json.stringify(
      "L'\xe2ge idiot"
    ),
    "\"L'\\uFFFDge idiot\""
  )
  test.equal(
    json.stringify(
      "Coraz\xf3n Serrano"
    ),
    "\"Coraz\\uFFFDn Serrano\""
  )
  test.equal(
    json.stringify(
      "\xe9t\xe9 ok"
    ),
    "\"\\uFFFDt\\uFFFD ok\""
  )
  test.equal(json.stringify("\xc3\xa9t\xc3\xa9"), "\"été\"")

  # string.escape falls back to ascii on invalid UTF-8: the bytes are kept.
  test.equal(
    string.escape(
      "Coraz\xf3n Serrano"
    ),
    "Coraz\\xF3n Serrano"
  )
  test.pass()
end

test.check(f)
