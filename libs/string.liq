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

# Value of an integer encoded using native memory representation.
# @category String
# @param ~little_endian Whether the memory representation is little endian.
# @param s String containing the binary representation.
def string.binary.to_int(~little_endian=true, s)
  ans = ref(0)
  n = string.length(s)
  for(0, n-1, fun (i) -> ans := lsl(!ans,8) + string.nth(s, if little_endian then n-1-i else i end))
  !ans
end

# Add a null character at the end of a string.
# @category String
# @param s String
def string.null_terminated(s)
  s ^ "\000"
end
