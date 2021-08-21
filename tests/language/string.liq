#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{json.stringify(x)} instead of #{json.stringify(y)}")
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
  t(string.match(pattern=".*ab.*","cccabc"), true)
  t(string.match(pattern=".*ab.*","cccbac"), false)
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
  s = string.extract(pattern="([^=]*)=(.*)", "ab=cde")
  t(s[1], "ab")
  t(s[2], "cde")
  t(string.is_int("123"), true)
  t(string.is_int("1a3"), false)
  t(string.unescape("\\a\\b\\e\\f\\n\\r\\t\\v\\'\\?\\x61\\141\\u0061"), "\a\b\e\f\n\r\t\v\'\?aaa")
  t(string.escape("\a\b\e\f\n\r\t\v\'\?"), "\\x07\\b\\x1b\\x0c\\n\\r\\t\\x0b'?")
  t(string.escape(encoding="ascii", "foo \"😅 o"), "foo \\\"\\xF0\\x9F\\x98\\x85 o")
  t("\xF0\x9F\x98\x85", "😅")
  t("\342\234\250", "✨")
  t("\u2728", "✨")
  t(string.escape.char("✨"), "\\u2728")
  t(string.escape.char(format="hex", "✨"), "\\xE2\\x9C\\xA8")
  t(string.escape.char(format="octal","✨"), "\\342\\234\\250")
  t(string.unescape("\\u2728"), "✨")
  t(string.unescape("\\xE2\\x9C\\xA8"), "✨")
  t(string.unescape("\\342\\234\\250"), "✨")
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
