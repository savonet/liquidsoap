# Test whether a string contains a given prefix, substring or suffix.
# @category String
# @param ~prefix Prefix to look for.
# @param ~substring Substring to look for.
# @param ~suffix Suffix to look for.
# @param s The string to look into.
def string.contains(~prefix="", ~substring="", ~suffix="", s)
  ans = ref(prefix == "" and substring == "" and suffix == "")

  if prefix != "" then
    ans := !ans or string.sub(s, start=0, length=string.length(prefix)) == prefix
  end

  if suffix != "" then
    suflen = string.length(suffix)
    ans := !ans or string.sub(s, start=string.length(s)-suflen, length=suflen) == suffix
  end

  if substring != "" then
    sublen = string.length(substring)
    def test(i)
      ans := !ans or (string.sub(s, start=i, length=sublen) == substring)
    end
    for(0, string.length(s)-sublen, test)
  end

  !ans
end
