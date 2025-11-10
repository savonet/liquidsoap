#!../../liquidsoap ../test.liq
def f() =
  test.equal(string.length(""), 0)
  test.equal(string.length("abc"), 3)
  test.equal(string.capitalize("aBc"), "ABc")
  test.equal(string.case("aBc"), "abc")
  test.equal(string.case(lower=false, "aBc"), "ABC")
  test.equal(string.concat(["ab", "c", "d"]), "abcd")
  test.equal(r/.*ab.*/.test("cccabc"), true)
  test.equal(r/.*ab.*/.test("cccbac"), false)
  test.equal(
    string.trim(
      "    abc  "
    ),
    "abc"
  )
  test.equal(string.sub("abcd", start=1, length=2), "bc")
  test.equal(string.sub("ab", start=0, length=10), "")
  test.equal(string.sub("ab", start=10, length=5), "")
  test.equal(string.contains(prefix="ab", "abcd"), true)
  test.equal(string.contains(prefix="ba", "abcd"), false)
  test.equal(string.contains(prefix="abcd", "ab"), false)
  test.equal(string.contains(suffix="cd", "abcd"), true)
  test.equal(string.contains(suffix="dc", "abcd"), false)
  test.equal(string.contains(substring="bc", "abcd"), true)
  test.equal(string.contains(substring="bc", "acbd"), false)
  test.equal(string.spaces(0), "")
  test.equal(
    string.spaces(4),
    "    "
  )
  test.equal(string.of_int(45), "45")
  test.equal(string.of_int(digits=4, 45), "0045")
  test.equal(string.binary.to_int(little_endian=true, "abcd"), 1684234849)
  test.equal(string.binary.to_int(little_endian=false, "abcd"), 1633837924)
  test.equal(string.binary.of_int(little_endian=true, 1684234849), "abcd")
  test.equal(string.binary.of_int(little_endian=false, 1633837924), "abcd")
  s = r/([^=]*)=(.*)/.exec("ab=cde")
  test.equal(s[1], "ab")
  test.equal(s[2], "cde")
  test.equal(
    string.unescape("\\/\\a\\b\\e\\f\\n\\r\\t\\v\\'\\?\\x61\\141\\u0061"),
    "/\a\b\e\f\n\r\t\v\'\?aaa"
  )

  test.equal(
    string.escape("/\\\a\b\e\f\n\r\t\v\'\?"),
    "/\\\\\\u0007\\b\\u001B\\u000C\\n\\r\\t\\u000B\\'?"
  )

  test.equal(
    string.escape(
      encoding="ascii",
      "\\foo \"😅 o"
    ),
    "\\\\foo \\\"\\xF0\\x9F\\x98\\x85 o"
  )

  test.equal("\xF0\x9F\x98\x85", "😅")
  test.equal("\342\234\250", "✨")
  test.equal("\u2728", "✨")
  test.equal("\/", "/")
  test.equal(string.escape.all("✨"), "\\u2728")
  test.equal(string.escape.all(format="hex", "✨"), "\\xE2\\x9C\\xA8")
  test.equal(string.escape.all(format="octal", "✨"), "\\342\\234\\250")
  test.equal(string.unescape("\\u2728"), "✨")
  test.equal(string.unescape("\\xE2\\x9C\\xA8"), "✨")
  test.equal(string.unescape("\\342\\234\\250"), "✨")
  test.equal(
    string.quote(
      encoding="ascii",
      "foo \"😅 o"
    ),
    "\"foo \\\"\\xF0\\x9F\\x98\\x85 o\""
  )

  test.equal(
    string.quote(
      "foo \"😅 o"
    ),
    "\"foo \\\"😅 o\""
  )
  test.equal(string.quote("aa'bb"), "\"aa'bb\"")
  test.equal(string.residual(prefix="ab", "abcd"), "cd")
  test.equal(string.residual(prefix="xx", "abcd"), null)
  test.equal(string.nth("abcde", 2), 99)
  test.equal(string.index(substring="cd", "abcde"), 2)
  test.equal(string.index(substring="e", "abcde"), 4)
  test.equal(string.index(substring="xx", "abcde"), -1)
  test.equal(string.split.first(separator="xx", "abcxxdef"), ("abc", "def"))

  # Test that we revert to ascii on invalid utf8 strings when encoding is not specified:
  invalid_utf8_string = "\xfc\xa1\xa1\xa1\xa1\xa1"
  test.equal(
    string.quote(invalid_utf8_string),
    string.quote(encoding="ascii", invalid_utf8_string)
  )

  # But raise an exception when encoding is specified:
  try
    ignore(string.quote(encoding="utf8", invalid_utf8_string))
    test.fail()
  catch _ : [error.string] do
    ()
  end

  test.equal(
    string.annotate.parse(
      "foo=bla,\"foo\"=\"✨✅\",\"gni:gno\"=\"bla\\\"blu:\":😈uri"
    ),
    ([("foo", "✨✅"), ("gni:gno", "bla\"blu:")], "😈uri")
  )

  test.equal(
    string.data_uri.encode(mime="foo/bar", "✨"),
    "data:foo/bar;base64,4pyo"
  )

  test.equal(
    string.data_uri.encode(base64=false, mime="foo/bar", "✨"),
    "data:foo/bar,✨"
  )

  test.equal(
    string.data_uri.decode("data:foo/bar;base64,4pyo"),
    null("✨".{mime="foo/bar"})
  )

  test.equal(
    string.data_uri.decode("data:foo/bar,✨"),
    null("✨".{mime="foo/bar"})
  )

  test.equal(
    url.encode(
      "foo bla blo"
    ),
    "foo+bla+blo"
  )
  test.equal(
    url.decode("foo%20bla%20blo"),
    "foo bla blo"
  )
  test.equal(
    "blo#{(1, 2, 3)}",
    "blo(1, 2, 3)"
  )

  s = "王^小東="
  test.equal(string.length(s), 5)
  test.equal(string.chars(s), ["王", "^", "小", "東", "="])
  test.equal(string.sub(start=1, length=2, s), "^小")
  test.equal(string.length(encoding="ascii", s), 11)
  test.equal(
    string.chars(encoding="ascii", s),
    [
      "\xE7",
      "\x8E",
      "\x8B",
      "^",
      "\xE5",
      "\xB0",
      "\x8F",
      "\xE6",
      "\x9D",
      "\xB1",
      "="
    ]
  )
  test.equal(string.sub(encoding="ascii", start=1, length=2, s), "\x8E\x8B")

  try
    ignore(string.chars(encoding="utf16le", s))
    test.fail()
  catch e : [error.invalid] do
    ()
  end

  test.equal(string.escape.html("&"), "&amp;")
  test.equal(string.escape.html("<"), "&lt;")
  test.equal(string.escape.html(">"), "&gt;")
  test.equal(string.escape.html('"'), "&quot;")
  test.equal(string.escape.html("'"), "&#39;")
  test.equal(string.escape.html("&<>\"'"), "&amp;&lt;&gt;&quot;&#39;")
  test.equal(
    string.escape.html(
      "not escaped"
    ),
    "not escaped"
  )
  test.equal(
    string.escape.html(
      "&quot;double escape&quot;"
    ),
    "&amp;quot;double escape&amp;quot;"
  )

  test.equal(string.escape.html("\\"), "\\")
  test.equal(string.escape.html("/"), "/")
  test.equal(string.escape.html("`"), "`")

  s = "blablo"
  flag = string.flags.binary(s)
  test.equal(flag(), false)
  flag := true
  test.equal(flag(), true)
  test.equal("#{s}", "blablo")
  test.equal(string(s), "blablo")
  test.equal(string(print_binary=false, s), "<string>")
  flag := false
  test.equal(flag(), false)
  test.equal("#{s}", "blablo")
  test.equal(string(s), "blablo")
  test.equal(string(print_binary=false, s), "blablo")

  test.pass()
end

test.check(f)
