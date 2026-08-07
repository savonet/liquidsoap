# A doc comment attaches to the statement that follows it, and ends up on the
# binding as `Doc.Value.t`. This is what `--list-functions-md` reports.

# Adds one to its argument.
# @category Programming
def documented(x) =
  x + 1
end

# Also documented, as a plain binding.
plain = 1

undocumented = 2

(documented(plain), undocumented)
