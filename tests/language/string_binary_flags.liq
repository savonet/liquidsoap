def f() =
  # Mark a string as binary.
  s =
    "binary data"
  string.flags.binary(s) := true

  # A freshly created string with the same content should automatically
  # inherit the binary flag via String_flags_map.
  s2 =
    "binary data"
  b = string.flags.binary(s2)
  test.equal(b(), true)

  # string() should mask binary content when print_binary=false.
  test.equal(string(print_binary=false, s2), "<string>")

  test.pass()
end

test.check(f)
