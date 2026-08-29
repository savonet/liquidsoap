# Create a reference from a pair of get / set functions. Every operation runs as
# an atomic section, so the reference is indivisible against other sections
# rather than against everything.
# @category Programming
# @param get Function to retrieve the value of the reference.
# @param set Function to change the value of the reference.
def ref.make(get, set) =
  def exchange(v) =
    def replace() =
      previous = get()
      set(v)
      previous
    end
    atomic(replace)
  end
  def set(v) =
    atomic({set(v)})
  end
  def get() =
    atomic(get)
  end
  (get.{set = set, exchange = exchange} : ref)
end

# Create a getter from a reference (sometimes useful to remove the `set`
# method).
# @category Programming
def ref.getter((r:ref)) =
  {r()}
end

# Increment a reference to an integer.
# @category Programming
def ref.incr(r) =
  r := r() + 1
end

# Map functions to a reference.
# @category Programming
# @param g Function to apply to the getter.
# @param s Function to apply to the setter.
def ref.map(g, s, (r:ref)) =
  ref.make({g(r())}, fun (x) -> r.set(s(x)))
end
