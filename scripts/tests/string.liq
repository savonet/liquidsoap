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

if !success then test.pass() else test.fail() end