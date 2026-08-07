# A binding scopes over the rest of its block, in every block position.
def outer() =
  a = 1
  a + 1
end

inner_if =
  if
    true
  then
    b = 2
    b
  else
    0
  end

def last_statement() =
  # A binding as the last statement of a block: nothing follows it, so the
  # block evaluates to unit.
  c = 3
end

(outer(), inner_if, last_statement())
