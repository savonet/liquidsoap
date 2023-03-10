# Add an element at the top of a list.
# @category List
def list.cons(x,l) =
  list.add(x,l)
end

# "Delayed" version of `list.case` where the value on empty list is only
# evaluated if necessary.
# @category List
def list.dcase(l, d, f) =
  f = list.case(l, d, fun (x,l) -> {f(x,l)})
  f()
end

# Return the head (first element) of a list, or `default` if the list is empty.
# @category List
# @param ~default Default value if key does not exist.
def list.hd(~default=null(), l)
  list.dcase(l, {default ?? error.raise(error.not_found, "no default value for list.hd")}, fun (x, _) -> x)
end

# Return the list without its first element.
# @category List
def list.tl(l)
  list.case(l, [], fun (_, l) -> l)
end

# Initialize a list.
# @category List
# @param n Number of elements in the list.
# @param f Function such that `f(i)` is the `i`th element.
def list.init(n, f)
  def rec aux(cur, i)
    cur = list.add(f(i), cur)
    if i == 0 then cur else
      aux(cur, i-1)
    end
  end
  if n < 0 then [] else aux([], n-1) end
end

# Create a list with given length, filled with given element.
# @category List
# @param n Number of elements in the list.
# @param x Element to fill the list with.
def list.make(n, x)
  list.init(n, fun(_)->x)
end

# Get the n-th element of a list (the first element is at position 0), or `default` if element does not exist.
# @category List
def list.nth(~default=null(), l, n) =
  def rec aux(l, n)
    list.dcase(l, {default ?? error.raise(error.not_found, "no default value for list.nth")}, fun (x, l) -> if n == 0 then x else aux(l, n-1) end)
  end
  aux(l, n)
end

# Determining whether a list is empty or not.
# @category List
def list.is_empty(l)
  list.case(l, true, fun (_, _) -> false)
end

# Compute the length of a list, i.e., the number of its elements.
# @category List
def list.length(l)
  list.ind(l, 0, fun (_, _, r) -> r+1)
end

# Return the last element of a list.
# @category List
def list.last(~default=null(), l)
  list.nth(default=default, l, list.length(l)-1)
end

# Check whether an element belongs to a list.
# @category List
def list.mem(x, l)
  def rec aux(l)
    list.case(l, false, fun (y, l) -> x == y or aux(l))
  end
  aux(l)
end

# Call a function on every element of a list.
# @category List
def list.iter(f, l)
  def rec aux(l)
    list.case(l, (), fun (x, l) -> begin (f(x):unit); aux(l) end)
  end
  aux(l)
end

# Call a function on every element of a list, along with its index
# @category List
def list.iteri(f, l)
  n = ref(0)
  def rec aux(l)
    i = n()
    n := i + 1
    list.case(l, (), fun (x, l) -> begin (f(i, x):unit); aux(l) end)
  end
  aux(l)
end

# Map a function on every element of a list.
# @category List
def list.map(f, l)
  def rec aux(l)
    list.case(l, [], fun (x, l) -> list.cons(f(x), aux(l)))
  end
  aux(l)
end

# Map a function on every element of a list, starting from the right. This
# function is tail-recursive.
# @category List
def list.map.right(f, l)
  list.ind(l, [], fun(x, _, l) -> list.cons(f(x), l))
end

# Map a function on every element of a list, along with its index.
# @category List
def list.mapi(f, l)
  n = ref(0)
  def f(x) =
    i = n()
    n := i + 1
    f(i, x)
  end
  list.map(f, l)
end

# Add indices to every element of a list, so that it can be accessed with the
# notation `l[n]`.
# @category List
def list.indexed(l)
  list.mapi(fun(i, x) -> (i,x), l)
end

# Fold a function on every element of a list: `list.fold(f,x1,[e1,..,en]) is f(...f(f(x1,e1),e2)...,en)`.
# @category List
# @param f Function `f` for which `f(x,e)` which will be called on every element `e` with the current value of `x`, returning the new value of `x`.
# @param x Initial value x1, to be updated by successive calls of `f(x,e)`.
def list.fold(f, x, l)
  def rec aux(x, l)
    list.case(l, x, fun (e, l) -> aux(f(x, e), l))
  end
  aux(x, l)
end

# Fold a function on every element of a list. Similar to `list.fold` but
# iterates from the right of the list. It is slightly more efficient than
# `list.fold`.
# @category List
# @param f Function `f` for which `f(x,e)` which will be called on every element `e` with the current value of `x`, returning the new value of `x`.
# @param x Initial value x1, to be updated by successive calls of `f(x,e)`.
def list.fold.right(f, x, l)
  list.ind(l, x, fun (e, _, r) -> f(e, r))
end

# Filter a list according to a predicate. The order in which elements are
# handled is not specified (and is currently implemented from the right).
# @category List
# @param ~remove Function called on an element when it is removed.
# @param p Predicate indicating whether an element should be kept or not.
# @param l List to filter.
def list.filter(~remove=fun(_)->(), p, l)
  # list.case(l, [], fun (x, l) -> if p(x) then list.cons(x, list.filter(p, l)) else list.filter(p, l) end)
  list.ind(l, [], fun(x, _, l) -> if p(x) then list.cons(x, l) else (remove(x):unit); l end)
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
def list.append(l, m)
  list.ind(l, m, fun (x, _, r) -> list.cons(x, r))
end

# Add a new last element to the list.
# @category List
# @flag hidden
def list.snoc(y, l)
  list.ind(l, list.cons(y, []), fun (x, _, r) -> list.cons(x, r))
end

# Revert list order.
# @category List
def list.rev(l)
  list.ind(l, [], fun (x, _, r) -> list.snoc(x, r))
end

# Associate a value to a key in an association list. This functions raises
# `error.not_found` if no default value is specified.
# @category List
# @param ~default Value returned if the key is not found.
def list.assoc(~default=null(), key, l)
  def rec aux(l)
    def f(x, l)
      let (k, v) = x
      if k == key then v else aux(l) end
    end
    list.dcase(l, {default ?? error.raise(error.not_found, "no default value for list.assoc")}, f)
  end
  aux(l)
end

# Keep only the elements of an association list satisfying a given predicate.
# @category List
def list.assoc.filter(p, l)
  def p(kv) = p(fst(kv), snd(kv)) end
  list.filter(p, l)
end

# Remove the first pair from an associative list.
# @category List
# @param key Key of pair to be removed.
# @param l List of pairs (key,value).
def list.assoc.remove(key, l)
  def rec aux(l)
    def f(x, l)
      let (k, v) = x
      if k == key then l else list.cons((k,v), aux(l)) end
    end
    list.dcase(l, {error.raise(error.not_found, "inexistent key in list.assoc.remove")}, f)
  end
  aux(l)
end

# Remove all pairs with given key from an associative list.
# @category List
# @param key Key of pairs to be removed.
# @param l List of pairs (key,value).
def list.assoc.remove.all(key, l)
  list.assoc.filter(fun(k,_) -> k != key, l)
end

# Check that a predicate is satisfied for every element in a list.
# @category List
# @param p Predicate.
# @param l List
def list.for_all(p, l)
  def rec aux(l)
    def f(x, l)
      if not p(x) then
        false
      else
        aux(l)
      end
    end
    list.case(l, true, f)
  end
  aux(l)
end

# Check that a predicate is satisfied for some element in a list.
# @category List
# @param p Predicate.
# @param l List
def list.exists(p, l)
  def rec aux(l)
    def f(x, l)
      if p(x) then
        true
      else
        aux(l)
      end
    end
    list.case(l, false, f)
  end
  aux(l)
end

# First element satisfying a predicate. Raises `error.not_found` if not element
# is found and no default value was specified.
# @category List
# @param ~default Returned value when the predicate is not found.
# @param p Predicate.
# @param l List
def list.find(~default=null(), p, l)
  def rec aux(l)
    list.dcase(l, {default ?? error.raise(error.not_found, "no default value for list.find")}, fun(x, l) -> if p(x) then x else aux(l) end)
  end
  aux(l)
end

# First index where a predicate is satisfied.
# @category List
# @param p Predicate.
# @param l List
def list.index(p, l)
  list.ind(l, 0, fun (x, _, r) -> if p(x) then 0 else r+1 end)
end

# list.assoc.mem(key,l) returns true if l contains a pair (key,value).
# @category List
# @param a Key to look for
# @param l List of pairs (key,value)
def list.assoc.mem(a,l)
  def f(cur, el) =
    if not cur then
      fst(el) == a
    else
      cur
    end
  end
  list.fold(f, false, l)
end

# Create an iterator over the elements of a list.
# @category List
def list.iterator(l)
  l = ref(l)
  def f()
    list.case(l(), null(), fun (x,t) -> begin l := t; x end)
  end
  f
end

# Returns a copy of the given list with a new element inserted at a given
# position. Raises `error.not_found` if the list has less than `index` elements.
# @category List
# @param index Index to insert at, starting at `0`.
# @param new_element Element to insert
# @param l List to insert into.
def list.insert(index, new_element, l) =
  if list.length(l) < index then
    error.raise(error.not_found, "List should have at least #{index} elemments")
  end

  if index == 0 then
    new_element::l
  else
    def f(cur, el) =
      let (pos, l) = cur
      l = if pos+1 == index then
          new_element::el::l
        else
          el::l
        end
      (pos+1, l)
    end

    let (_, l) = list.fold(f, (0, []), l)
    list.rev(l)
  end
end

# Compute the beginning of a list.
# @category List
# @param n Number of elements in the returned list.
# @param l List whose prefix should be taken.
def list.prefix(n, l)
  def rec aux(n, l)
    if n <= 0 or list.is_empty(l) then [] else
      list.hd(l)::aux(n-1, list.tl(l))
    end
  end
  aux(n, l)
end

# Pick a random element in a list.
# @category List
# @param ~default Value returned if the list is empty.
# @param l List in which the element should be picked.
def list.pick(~default=null(), l)
  if list.is_empty(l) then
    default ?? error.raise(error.not_found, "empty list in list.pick")
  else
    list.nth(l, random.int(min=0, max=list.length(l)))
  end
end

# Sort a list according to the "natural" order.
# @category List
# @param l List to sort
def list.sort.natural(l)
  def compare(x, y)
    if x<y then -1
    elsif x>y then 1
    else 0 end
  end
  list.sort(compare, l)
end
