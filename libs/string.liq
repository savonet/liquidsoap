# Test whether a string contains a given substring.
# @category String
# @param ~substring The substring to look for.
# @param s The string to look into.
def string.contains(~substring, s)
  ans = ref(false)
  sublen = string.length(substring)
  def test(i)
    ans := !ans or (string.sub(s, start=i, length=sublen) == substring)
  end
  for(0, string.length(s)-sublen, test)
  !ans
end
