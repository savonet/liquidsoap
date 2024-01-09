#!../../liquidsoap ../test.liq

def f() =
  test.equals(1::2::[3, 4], [1, 2, 3, 4])
  test.equals(list.hd(default=0, []), 0)
  test.equals(list.hd([5, 6]), 5)
  test.equals(
    try
      list.hd([])
    catch _ do
      9
    end,
    9
  )

  test.equals(list.tl([]), [])
  test.equals(list.tl([4, 5, 6]), [5, 6])
  test.equals(list.nth(default=1, [4], 5), 1)
  test.equals(list.nth([4, 5, 6, 7], 2), 6)
  test.equals(list.last(default=1, []), 1)
  test.equals(list.last([12, 5, 8]), 8)
  test.equals(list.length([]), 0)
  test.equals(list.length([1, 2, 3]), 3)
  test.equals(list.length(["a", "b"]), 2)
  test.equals(list.mem(1, [4, 5, 6]), false)
  test.equals(list.mem(5, [4, 5, 6]), true)
  test.equals(list.assoc.mem(1, [(4, "bla"), (5, "foo"), (6, "gni")]), false)
  test.equals(list.assoc.mem(5, [(4, "bla"), (5, "foo"), (6, "gni")]), true)
  test.equals(list.map(fun (x) -> 2 * x, [1, 2, 3]), [2, 4, 6])
  test.equals(list.map(fun (x) -> 2. * x, [1., 2., 3.]), [2., 4., 6.])
  test.equals(list.map.right(fun (x) -> 2 * x, [1, 2, 3]), [2, 4, 6])
  test.equals(list.mapi(fun (i, x) -> i + x, [1, 1, 2]), [1, 2, 4])
  test.equals(list.filter(fun (x) -> x mod 2 == 0, [1, 2, 3, 4, 5]), [2, 4])
  test.equals(list.remove(1, [3, 1, 2, 1, 1]), [3, 2, 1, 1])
  test.equals(list.remove(1, [3, 2]), [3, 2])
  test.equals(list.append([1, 2], [3, 4, 5]), [1, 2, 3, 4, 5])
  test.equals(list.rev([1, 2, 3]), [3, 2, 1])
  test.equals(list.assoc(default=0, "", [("a", 1), ("b", 2)]), 0)
  test.equals(list.assoc.nullable("b", [("a", 1), ("b", 2)]), 2)
  test.equals(list.assoc.nullable("x", [("a", 1), ("b", 2)]), null())
  test.equals(
    list.assoc.remove("a", [("b", 2), ("a", 1), ("a", 3)]), [("b", 2), ("a", 3)]
  )

  test.equals(
    list.assoc.remove.all("c", [("c", 1), ("b", 2), ("c", 2)]), [("b", 2)]
  )
  test.equals(
    list.assoc.remove.all("c", [("a", 1), ("b", 2)]), [("a", 1), ("b", 2)]
  )
  test.equals(list.for_all(fun (n) -> n > 0, []), true)
  test.equals(list.for_all(fun (n) -> n > 0, [1, 2, 3]), true)
  test.equals(list.for_all(fun (n) -> n > 0, [1, 0, 3]), false)
  test.equals(list.exists(fun (n) -> n > 0, [0, 2, 0]), true)
  test.equals(list.exists(fun (n) -> n > 0, [0, 0, 0]), false)
  test.equals(list.fold(fun (x, y) -> x + y, 1, [2, 3, 4]), 10)
  test.equals(list.fold.right(fun (x, y) -> x + y, 1, [2, 3, 4]), 10)
  test.equals(list.prefix(2, [1, 2, 3]), [1, 2])
  test.equals(list.prefix(3, [1]), [1])
  test.equals(list.init(4, fun (x) -> 2 * x), [0, 2, 4, 6])
  test.equals(list.index(fun (x) -> x == 0, [1, 2, 0, 3]), 2)
  test.equals(list.index(fun (x) -> x == 0, [1, 2, 3]), 3)
  test.equals(list.insert(0, 1, []), [1])
  test.equals(list.insert(0, 1, [2, 3, 4]), [1, 2, 3, 4])
  test.equals(list.insert(1, 2, [1, 3, 4]), [1, 2, 3, 4])
  test.equals(list.insert(4, 5, [1, 2, 3, 4]), [1, 2, 3, 4, 5])
  test.equals(list.flatten([[1, 2], [3, 4]]), [1, 2, 3, 4])

  def even(x) =
    (x / 2) * 2 == x
  end

  test.equals(list.find(default=42, even, [1, 3, 5]), 42)
  test.equals(list.find(even, [1, 3, 4, 5, 6]), 4)
  test.equals(list.sort.natural(["b", "c", "a"]), ["a", "b", "c"])
  l = list.indexed(["a", "b", "c"])
  test.equals(l[1], "b")
  l = [(1, "a"), (2, "b"), (3, "c")]
  test.equals(l[2], "b")
  test.equals(l[27], "")
  test.equals([1, ...[2, 3, 4], ...[5, 6], 7], [1, 2, 3, 4, 5, 6, 7])
  let [x, y] = [1, 2, 3]
  test.equals(x, 1)
  test.equals(y, 2)
  let [..._, x, y] = [1, 2, 3]
  test.equals(x, 2)
  test.equals(y, 3)
  let [x, _, ...z] = [1, 2, 3]
  test.equals(x, 1)
  test.equals(z, [3])

  # Ensure that common methods are kept and others are forgotten
  l = ["a".{b=true, k=0}, "b".{b=true, l=1}]
  test.equals(list.for_all(fun (x) -> x.b, l), true)
  _ = ["a".{b=true}, "c"]

  # There used to be a bug where [x] would loose all its methods.
  x = 1
  let x.foo = "bla"
  _ = list.add(x, [2.{foo="gni"}])
  try
    ignore(list.insert(23, 2, [1, 2, 3]))
    test.fail()
  catch err : [error.not_found] do
    test.equals(err.kind, "not_found")
    test.equals(
      err.message,
      "List should have at least 23 elemments"
    )
  end

  # Test huge lists, see #2162.
  # l = list.init(100000, fun(i) -> i)
  test.pass()
end

test.check(f)
