#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    let json.stringify x = x
    let json.stringify y = y
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def f() =
  t(string.length(""), 0)
  t(string.length("abc"), 3)
  t(string.capitalize("aBc"), "ABc")
  t(string.case("aBc"), "abc")
  t(string.case(lower=false, "aBc"), "ABC")
  t(string.concat(["ab","c","d"]), "abcd")
  t(r/.*ab.*/.test("cccabc"), true)
  t(r/.*ab.*/.test("cccbac"), false)
  t(string.trim("    abc  "), "abc")
  t(string.sub("abcd",start=1,length=2), "bc")
  t(string.sub("ab",start=0,length=10), "")
  t(string.sub("ab",start=10,length=5), "")
  t(string.contains(prefix="ab","abcd"), true)
  t(string.contains(prefix="ba","abcd"), false)
  t(string.contains(prefix="abcd","ab"), false)
  t(string.contains(suffix="cd","abcd"), true)
  t(string.contains(suffix="dc","abcd"), false)
  t(string.contains(substring="bc","abcd"), true)
  t(string.contains(substring="bc","acbd"), false)
  t(string.binary.to_int(little_endian=true,"abcd"),0x64636261)
  t(string.binary.to_int(little_endian=false,"abcd"),0x61626364)
  t(string.binary.of_int(little_endian=true,0x64636261),"abcd")
  t(string.binary.of_int(little_endian=false,0x61626364), "abcd")
  s = r/([^=]*)=(.*)/.exec("ab=cde")
  t(s[1], "ab")
  t(s[2], "cde")
  t(string.unescape("\\/\\a\\b\\e\\f\\n\\r\\t\\v\\'\\?\\x61\\141\\u0061"), "/\a\b\e\f\n\r\t\v\'\?aaa")
  t(string.escape("/\\\a\b\e\f\n\r\t\v\'\?"), "/\\\\\\u0007\\b\\u001B\\u000C\\n\\r\\t\\u000B\\'?")
  t(string.escape(encoding="ascii", "\\foo \"😅 o"), "\\\\foo \\\"\\xF0\\x9F\\x98\\x85 o")
  t("\xF0\x9F\x98\x85", "😅")
  t("\342\234\250", "✨")
  t("\u2728", "✨")
  t("\/", "/")
  t(string.escape.all("✨"), "\\u2728")
  t(string.escape.all(format="hex", "✨"), "\\xE2\\x9C\\xA8")
  t(string.escape.all(format="octal","✨"), "\\342\\234\\250")
  t(string.unescape("\\u2728"), "✨")
  t(string.unescape("\\xE2\\x9C\\xA8"), "✨")
  t(string.unescape("\\342\\234\\250"), "✨")
  t(string.quote(encoding="ascii", "foo \"😅 o"), "\"foo \\\"\\xF0\\x9F\\x98\\x85 o\"")
  t(string.quote("foo \"😅 o"), "\"foo \\\"😅 o\"")
  t(string.quote("aa'bb"), "\"aa'bb\"")

  t(string.annotate.parse("foo=bla,\"foo\"=\"✨✅\",\"gni:gno\"=\"bla\\\"blu:\":😈uri"),
    ([("foo", "bla"), ("foo", "✨✅"), ("gni:gno", "bla\"blu:")], "😈uri"))

  t(string.data_uri.encode(mime="foo/bar", "✨"), "data:foo/bar;base64,4pyo")
  t(string.data_uri.encode(base64=false, mime="foo/bar", "✨"), "data:foo/bar,✨")

  t(string.data_uri.decode("data:foo/bar;base64,4pyo"), null("✨".{mime = "foo/bar"}))
  t(string.data_uri.decode("data:foo/bar,✨"), null("✨".{mime = "foo/bar"}))

  if !success then test.pass() else test.fail() end
end

test.check(f)
