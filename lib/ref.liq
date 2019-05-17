# Create a reference, i.e. a value that can be modified. This is an alias to @ref@.
def ref.make(x) =
  ref x
end

# Obtain the value stored in a reference. This is an alias to @!@.
def ref.get(r) =
  !r
end

# Change the value stored in a reference. This is an alias to @:=@.
def ref.set(r, x) =
  r := x
end