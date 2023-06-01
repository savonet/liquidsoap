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

# Create a list with given length, filled with given element.
# @category List
# @param n Number of elements in the list.
# @param x Element to fill the list with.
def list.make(n, x)
  list.init(n, fun(_)->x)
end

# Determining whether a list is empty or not.
# @category List
def list.is_empty(l)
  list.case(l, true, fun (_, _) -> false)
end

# Return the last element of a list.
# @category List
def list.last(~default=null(), l)
  list.nth(default=default, l, list.length(l)-1)
end

# Call a function on every element of a list.
# @category List
def list.iter(f, l)
  list.iteri(fun (_, v) -> f(v), l)
end

# Check whether an element belongs to a list.
# @category List
def list.mem(x, l)
  err = error.register("mem")
  try
    list.iter(fun (v) -> if v == x then error.raise(err, "found") end, l)
    false
  catch _ : [err] do
    true
  end
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
  ret = ref(x)
  list.iter(fun (v) ->
    ret := f(ret(), v), l)
  ret()
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

# Concatenate all the elements of a list of lists.
# @category List
def list.flatten(l)
  list.fold(fun(l,s) -> list.append(l,s), [], l)
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

# Map a function on a list (like `list.map`) excepting that the value is removed
# if the function returns `null`.
# @category List
# @param f Function called on every element of the list.
# @param l The list.
def list.filter_map(f, l)
  def f(x, _, l)
    y = f(x)
    if null.defined(y) then
      list.cons(null.get(y), l)
    else
      l
    end
  end
  list.ind(l, [], f)
end

# Associate a value to a key in an association list. This functions raises
# `error.not_found` if no default value is specified.
# @category List
# @param ~default Value returned if the key is not found.
def list.assoc(~default=null(), key, l)
  ret = ref(💣())
  err = error.register("assoc")
  try
    list.iter(fun (v) ->
      if fst(v) == key then
        ret := snd(v)
        error.raise(err, "found")
      end, l)
    null.case(default, fun () -> error.raise(error.not_found, "no default value for list.assoc"), fun (v) -> v)
  catch _ : [err] do
    ret()
  end
end

# Keep only the elements of an association list satisfying a given predicate.
# @category List
def list.assoc.filter(p, l)
  def p(kv) = p(fst(kv), snd(kv)) end
  list.filter(p, l)
end

# Map a function of every element of the associative list, removing the entry if
# the function returns `null`.
# @category List
def list.assoc.filter_map(f, l)
  def f(kv) = f(fst(kv), snd(kv)) end
  list.filter_map(f, l)
end

# Remove the first pair from an associative list.
# @category List
# @param key Key of pair to be removed.
# @param l List of pairs (key,value).
def list.assoc.remove(key, l)
  ret = ref(💣())
  err = error.register("assoc.remove")
  try
    list.iter(fun (v) ->
      if fst(v) == key then
        ret := snd(v)
        error.raise(err, "found")
      end, l)
    l
  catch _ : [err] do
    list.remove((key, ret()), l)
  end
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
  err = error.register("for_all")
  try
    list.iter(fun (v) ->
      if not p(v) then
        error.raise(err, "found")
      end, l)
    true
  catch _ : [err] do
    false
  end
end

# Check that a predicate is satisfied for some element in a list.
# @category List
# @param p Predicate.
# @param l List
def list.exists(p, l)
  err = error.register("exists")
  try
    list.iter(fun (v) ->
     if p(v) then
       error.raise(err, "found")
     end, l)
    false
  catch _ : [err] do
    true
  end
end

# First element satisfying a predicate. Raises `error.not_found` if not element
# is found and no default value was specified.
# @category List
# @param ~default Returned value when the predicate is not found.
# @param p Predicate.
# @param l List
def list.find(~default=null(), p, l)
  ret = ref(💣())
  err = error.register("find")
  try
    list.iter(fun (v) ->
      if p(v) then
        ret := v
        error.raise(err, "found")
      end, l)
    null.case(default, fun () -> error.raise(error.not_found, "no default value for list.find"), fun (v) -> v)
  catch _ : [err] do
    ret()
  end
end

# First index where a predicate is satisfied.
# @category List
# @param p Predicate.
# @param l List
def list.index(p, l)
  list.ind(l, 0, fun (x, _, r) -> if p(x) then 0 else r+1 end)
end

# `list.assoc.mem(key,l)` returns `true` if `l` contains a pair (key,value).
# @category List
# @param a Key to look for
# @param l List of pairs (key,value)
def list.assoc.mem(a,l)
  err = error.register("find")
  try
    list.iter(fun (el) ->
      if fst(el) == a then
        error.raise(err, "found")
      end, l)
    false
  catch _ : [err] do
    true
  end
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
  len = list.length(l)
  n = n < len ? n : len
  list.init(n, fun (n) -> list.nth(l, n))
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
