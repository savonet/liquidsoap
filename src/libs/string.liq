# Match a string with an expression. Perl compatible regular expressions are
# recognized. Hence, special characters should be escaped. Alternatively, one
# can use the the `r/_/.test(_)` syntax for regular expressions.
# @category String
def string.match(~pattern, s) =
  regexp(pattern).test(s)
end

# Extract substrings from a string. Perl compatible regular expressions are
# recognized. Hence, special characters should be escaped. Returns a list of
# (index,value). If the list does not have a pair associated to some index, it
# means that the corresponding pattern was not found. Alter natively, one can
# use the `r/_/.exec(_)` syntax for regular expressions.
# @category String
def string.extract(~pattern, s) =
  regexp(pattern).exec(s)
end

# Replace all substrings matched by a pattern by another string returned by a
# function. Alternatively, one can use the `r/_/g.replace(_)` syntax for regular
# expressions.
# @category String
# @param ~pattern Pattern (regular expression) of substrings which should be replaced.
# @param f Function getting a matched substring an returning the string to replace it with.
# @param s String whose substrings should be replaced.
def string.replace(~pattern, f, s) =
  regexp(flags=["g"], pattern).replace(f, s)
end

# Split a string at "separator". Perl compatible regular expressions are
# recognized. Hence, special characters should be escaped. Alternatively, one
# can use the `r/_/.split(_)` syntax for regular expressions.
# @category String
def string.split(~separator, s) =
  regexp(separator).split(s)
end

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

# What remains of a string after a given prefix.
# @category String
# @param ~prefix Requested prefix.
def string.residual(~prefix, s)
  n = string.length(prefix)
  if string.contains(prefix=prefix, s) then
    string.sub(s, start=n, length=string.length(s)-n)
  else
    null()
  end
end

# Test whether a string is a valid integer.
# @category String
def string.is_int(s)
  string.match(pattern="^\\d+$", s)
end

# Convert a string to a int.
# @category String
def string.to_int(~default=0, s)
  int_of_string(default=default, s)
end

# Convert a string to a float.
# @category String
def string.to_float(~default=0., s)
  float_of_string(default=default, s)
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
# @param ~encoding One of: `"ascii"` or `"utf8"`.
def string.quote(~encoding="utf8", s) =
  def special_char(~encoding=encoding, s)
    if s == "'" then
      false
    else
      string.escape.special_char(encoding=encoding, s)
    end
  end
  s = string.escape(special_char=special_char, encoding=encoding, s)
  "\"#{s}\""
end

let string.data_uri = ()

# Encode a string using the data uri format,
# i.e. `"data:<mime>[;base64]:<data>"`.
# @category String
# @param ~base64 Encode data using the base64 format
# @param ~mime Data mime type
def string.data_uri.encode(~base64=true, ~(mime:string), s) =
  s = base64 ? ";base64:#{string.base64.encode(s)}" : ":#{s}"
  "data:#{mime}#{s}"
end

# Decode a string using the data uri format,
# i.e. `"data:<mime>[;base64]:<data>"`.
# @category String
def string.data_uri.decode(s) =
  captured = r/^data:([\/\w]+)(;base64)?:(.+)$/.exec(s)
  if list.assoc.mem(1, captured) and list.assoc.mem(3, captured) then
    mime = list.assoc(1, captured)
    data = list.assoc(3, captured)
    data =
      if list.assoc.mem(2, captured) then
        string.base64.decode(data)
      else
        data
      end 
    data.{mime=mime}
  else
    null()
  end
end
