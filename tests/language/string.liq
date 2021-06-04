#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
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
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
