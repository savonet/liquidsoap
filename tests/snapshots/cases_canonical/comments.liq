# The `comments` array: raw text plus absolute cnum offsets, reported
# independently of the AST so prettier can place them itself.

# A doc comment on a binding.
x = 1 # trailing, same line

# Leading a block.
def g() =
  # Inside the block.
  x
end

g()
