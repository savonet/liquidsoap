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
    for i = 0 to string.length(s)-sublen do
      ans := !ans or (string.sub(s, start=i, length=sublen) == substring)
    end
  end

  !ans
end

# Test whether a string is a valid integer.
# @category String
def string.is_int(s)
  string.match(pattern="^\\d+$", s)
end

let string.binary = ()

# Value of a positive (unsigned) integer encoded using native memory representation.
# @category String
# @param ~little_endian Whether the memory representation is little endian.
# @param s String containing the binary representation.
def string.binary.to_int(~little_endian=true, s)
  ans = ref(0)
  n = string.length(s)
  for i = 0 to n-1 do
    ans := lsl(!ans,8) + string.nth(s, if little_endian then n-1-i else i end)
  end
  !ans
end

# Encode a positive (unsigned) integer using native memory representation.
# @category String
# @param ~pad Minimum length in digits (pad on the left with zeros in order to reach it)
# @param ~little_endian Whether the memory representation is little endian.
# @param s String containing the binary representation.
def string.binary.of_int(~pad=0, ~little_endian=true, d)
  def rec f(d, s) =
    if d > 0 then
      c = string.hex_of_int(pad=2, (d mod 256))
      if little_endian then
        f(lsr(d, 8), "#{s}\\x#{c}")
      else
        f(lsr(d, 8), "\\x#{c}#{s}")
      end
    else
      s
    end
  end
  ret = d == 0 ? "\\x00" : f(d, "")
  ret = string.unescape(ret)
  len = string.length(ret)
  if len < pad then
    ans = string.make(char_code=0, pad-len)
    if little_endian then
      "#{ret}#{ans}"
    else
      "#{ans}#{ret}"
    end
  else
    ret
  end
end

# Add a null character at the end of a string.
# @category String
# @param s String.
def string.null_terminated(s)
  s ^ "\000"
end

# Generate an identifier if no identifier was provided.
# @category String
# @param ~default Name from which identifier is generated if not present.
# @param id Proposed identifier.
def string.id.default(~default, id)
  null.default(id, {string.id(default)})
end

# Return a quoted copy of the given string.
# By default, the string is assumed to be `"utf8"` encoded and is escaped
# following JSON and javascript specification.
# @category String
# @argsof string.escape.special_char
def string.quote(%argsof(string.escape.special_char), s) =
  # Escape all default special chars plus double quote
  def special_char(s) =
     s == "\"" or string.escape.special_char(%argsof(string.escape.special_char), s)
  end
  s = string.escape(%argsof(string.escape.special_char), special_char=special_char, s)
  "\"#{s}\""
end
