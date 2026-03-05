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

  # Removing the binary flag should update the map: new strings with the same
  # content should no longer inherit it.
  string.flags.binary(s) := false
  b_removed = string.flags.binary(s)
  test.equal(b_removed(), false)
  s3 =
    "binary data"
  b3 = string.flags.binary(s3)
  test.equal(b3(), false)

  test.pass()
end

test.check(f)
