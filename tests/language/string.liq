#!../../liquidsoap ../test.liq

def f() =
  test.equals(string.length(""), 0)
  test.equals(string.length("abc"), 3)
  test.equals(string.capitalize("aBc"), "ABc")
  test.equals(string.case("aBc"), "abc")
  test.equals(string.case(lower=false, "aBc"), "ABC")
  test.equals(string.concat(["ab","c","d"]), "abcd")
  test.equals(r/.*ab.*/.test("cccabc"), true)
  test.equals(r/.*ab.*/.test("cccbac"), false)
  test.equals(string.trim("    abc  "), "abc")
  test.equals(string.sub("abcd",start=1,length=2), "bc")
  test.equals(string.sub("ab",start=0,length=10), "")
  test.equals(string.sub("ab",start=10,length=5), "")
  test.equals(string.contains(prefix="ab","abcd"), true)
  test.equals(string.contains(prefix="ba","abcd"), false)
  test.equals(string.contains(prefix="abcd","ab"), false)
  test.equals(string.contains(suffix="cd","abcd"), true)
  test.equals(string.contains(suffix="dc","abcd"), false)
  test.equals(string.contains(substring="bc","abcd"), true)
  test.equals(string.contains(substring="bc","acbd"), false)
  test.equals(string.binary.to_int(little_endian=true,"abcd"),0x64636261)
  test.equals(string.binary.to_int(little_endian=false,"abcd"),0x61626364)
  test.equals(string.binary.of_int(little_endian=true,0x64636261),"abcd")
  test.equals(string.binary.of_int(little_endian=false,0x61626364), "abcd")
  s = r/([^=]*)=(.*)/.exec("ab=cde")
  test.equals(s[1], "ab")
  test.equals(s[2], "cde")
  test.equals(string.unescape("\\/\\a\\b\\e\\f\\n\\r\\t\\v\\'\\?\\x61\\141\\u0061"), "/\a\b\e\f\n\r\t\v\'\?aaa")
  test.equals(string.escape("/\\\a\b\e\f\n\r\t\v\'\?"), "/\\\\\\u0007\\b\\u001B\\u000C\\n\\r\\t\\u000B\\'?")
  test.equals(string.escape(encoding="ascii", "\\foo \"😅 o"), "\\\\foo \\\"\\xF0\\x9F\\x98\\x85 o")
  test.equals("\xF0\x9F\x98\x85", "😅")
  test.equals("\342\234\250", "✨")
  test.equals("\u2728", "✨")
  test.equals("\/", "/")
  test.equals(string.escape.all("✨"), "\\u2728")
  test.equals(string.escape.all(format="hex", "✨"), "\\xE2\\x9C\\xA8")
  test.equals(string.escape.all(format="octal","✨"), "\\342\\234\\250")
  test.equals(string.unescape("\\u2728"), "✨")
  test.equals(string.unescape("\\xE2\\x9C\\xA8"), "✨")
  test.equals(string.unescape("\\342\\234\\250"), "✨")
  test.equals(string.quote(encoding="ascii", "foo \"😅 o"), "\"foo \\\"\\xF0\\x9F\\x98\\x85 o\"")
  test.equals(string.quote("foo \"😅 o"), "\"foo \\\"😅 o\"")
  test.equals(string.quote("aa'bb"), "\"aa'bb\"")
  test.equals(string.residual(prefix="ab", "abcd"), "cd")
  test.equals(string.residual(prefix="xx", "abcd"), null())
  test.equals(string.nth("abcde", 2), 99)
  test.equals(string.index(substring="cd", "abcde"), 2)
  test.equals(string.index(substring="e", "abcde"), 4)
  test.equals(string.index(substring="xx", "abcde"), -1)
  test.equals(string.split.first(separator="xx", "abcxxdef"), ("abc","def"))

  # Test that we revert to ascii on invalid utf8 strings when encoding is not specified:
  invalid_utf8_string = "\xfc\xa1\xa1\xa1\xa1\xa1"
  test.equals(string.quote(invalid_utf8_string), string.quote(encoding="ascii", invalid_utf8_string))

  # But raise an exception when encoding is specified:
  try
    ignore(string.quote(encoding="utf8", invalid_utf8_string))
    test.fail()
  catch _ : [error.string] do
    ()
  end

  test.equals(string.annotate.parse("foo=bla,\"foo\"=\"✨✅\",\"gni:gno\"=\"bla\\\"blu:\":😈uri"),
    ([("foo", "bla"), ("foo", "✨✅"), ("gni:gno", "bla\"blu:")], "😈uri"))

  test.equals(string.data_uri.encode(mime="foo/bar", "✨"), "data:foo/bar;base64,4pyo")
  test.equals(string.data_uri.encode(base64=false, mime="foo/bar", "✨"), "data:foo/bar,✨")

  test.equals(string.data_uri.decode("data:foo/bar;base64,4pyo"), null("✨".{mime = "foo/bar"}))
  test.equals(string.data_uri.decode("data:foo/bar,✨"), null("✨".{mime = "foo/bar"}))

  test.equals(url.encode("foo bla blo"), "foo+bla+blo")
  test.equals(url.decode("foo%20bla%20blo"), "foo bla blo")

  test.pass()
end

test.check(f)

def test_escape_html() =
  test.equals(string.escape.html("&"), "&amp;")
  test.equals(string.escape.html("<"), "&lt;")
  test.equals(string.escape.html(">"), "&gt;")
  test.equals(string.escape.html('"'), "&quot;")
  test.equals(string.escape.html("'"), "&#39;")
  test.equals(string.escape.html("&<>\"'"), "&amp;&lt;&gt;&quot;&#39;")
  test.equals(string.escape.html("not escaped"), "not escaped")
  test.equals(string.escape.html("&quot;double escape&quot;"), "&amp;quot;double escape&amp;quot;")
  test.equals(string.escape.html("\\"), "\\")
  test.equals(string.escape.html("/"), "/")
  test.equals(string.escape.html("`"), "`")
end

test.check(test_escape_html)
