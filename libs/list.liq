list.cons = list.add

# Return the head (first element) of a list, or `default` if the list is empty.
# @category List
# @param ~default Default value if key does not exist.
def list.hd(~default, l)
  list.case(l, default, fun (x, _) -> x)
end

# Return the list without its first element.
# @category List
def list.tl(l)
  list.case(l, [], fun (_, l) -> l)
end

# Get the n-th element of a list (the first element is at position 0), or `default` if element does not exist.
# @category List
def rec list.nth(~default, l, n)
  list.case(l, default, fun (x, l) -> if n == 0 then x else list.nth(default=default, l, n-1) end)
end

# Determing whether a list is empty or not.
# @category List
def list.is_empty(l)
  list.case(l, true, fun (_, _) -> false)
end

# Compute the length of a list, i.e., the number of its elements.
# @category List
def rec list.length(l)
  list.case(l, 0, fun (_, l) -> list.length(l)+1)
end

# Check whether an element belongs to a list.
# @category List
def rec list.mem(x, l)
  list.case(l, false, fun (y, l) -> x == y or list.mem(x, l))
end

# Call a function on every element of a list.
# @category List
def rec list.iter(f, l)
  list.case(l, (), fun (x, l) -> begin f(x); list.iter(f, l) end)
end

# Map a function on every element of a list.
# @category List
def rec list.map(f, l)
  list.case(l, [], fun (x, l) -> list.cons(f(x), list.map(f, l)))
end

# Map a function on every element of a list, along with its index.
# @category List
def rec list.mapi(f, l)
  n = ref(0)
  def f(x) =
    i = !n
    n := i + 1
    f(i, x)
  end
  list.map(f, l)
end

# Fold a function on every element of a list: `list.fold(f,x1,[e1,..,en]) is f(...f(f(x1,e1),e2)...,en)`.
# @category List
# @param f Function `f` for which `f(x,e)` which will be called on every element `e` with the current value of `x`, returning the new value of `x`.
# @param x Initial value x1, to be updated by successive calls of `f(x,e)`.
def rec list.fold(f, x, l)
  list.case(l, x, fun (e, l) -> list.fold(f, f(x, e), l))
end

# Filter a list according to a filtering function.
# @category List
def rec list.filter(p, l)
  list.case(l, [], fun (x, l) -> if p(x) then list.cons(x, list.filter(p, l)) else list.filter(p, l) end)
end

# Remove the first occurrence of a value from a list.
# @category List
def list.remove(x, l)
  def rec aux(k, l)
    list.case(l, k([]),
    fun (y, l) -> if x == y then k(l) else aux(fun (l) -> k(list.cons(y, l)), l) end)
  end
  aux(fun (l) -> l, l)
end

# Concatenate two lists.
# @category List
def rec list.append(l, m)
  list.case(l, m, fun (x, l) -> list.cons(x, list.append(l, m)))
end

# Add a new last element to the list.
# @category List
# @flag hidden
def rec list.snoc(y, l)
  list.case(l, list.cons(y, []), fun (x, l) -> list.cons(x, list.snoc(y, l)))
end

# Revert list order.
# @category List
def rec list.rev(l)
  list.case(l, [], fun (x, l) -> list.snoc(x, list.rev(l)))
end

# Associate a value to a key in an association list.
# @category List
# @param ~default Value returned if the key is not found
def rec list.assoc(~default, key, l)
  def f(x, l)
    let (k, v) = x
    if k == key then v else list.assoc(default=default, key, l) end
  end
  list.case(l, default, f)
end

# Remove the first pair from an associative list.
# @category List
# @param key Key of pair to be removed.
# @param l List of pairs (key,value).
def rec list.remove_assoc(key, l)
  def f(x, l)
    let (k, v) = x
    if k == key then l else list.cons((k,v), list.remove_assoc(key, l)) end
  end
  list.case(l, [], f)
end
