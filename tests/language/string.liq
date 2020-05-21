#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

test(string.length(""), 0)
test(string.length("abc"), 3)
test(string.capitalize("aBc"), "ABc")
test(string.case("aBc"), "abc")
test(string.case(lower=false, "aBc"), "ABC")
test(string.concat(["ab","c","d"]), "abcd")
test(string.match(pattern=".*ab.*","cccabc"), true)
test(string.match(pattern=".*ab.*","cccbac"), false)
test(string.trim("    abc  "), "abc")
test(string.sub("abcd",start=1,length=2), "bc")
test(string.sub("ab",start=0,length=10), "")
test(string.sub("ab",start=10,length=5), "")
test(string.contains(prefix="ab","abcd"), true)
test(string.contains(prefix="ba","abcd"), false)
test(string.contains(prefix="abcd","ab"), false)
test(string.contains(suffix="cd","abcd"), true)
test(string.contains(suffix="dc","abcd"), false)
test(string.contains(substring="bc","abcd"), true)
test(string.contains(substring="bc","acbd"), false)
test(string.binary.to_int(little_endian=true,"abcd"),0x64636261)
test(string.binary.to_int(little_endian=false,"abcd"),0x61626364)

if !success then test.pass() else test.fail() end
