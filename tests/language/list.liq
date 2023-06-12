#!../../liquidsoap ../test.liq

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def f() =
  t(1::2::[3,4], [1,2,3,4])
  t(list.hd(default=0,[]), 0)
  t(list.hd([5,6]), 5)
  t(try list.hd([]) catch _ do 9 end, 9)
  t(list.tl([]), [])
  t(list.tl([4,5,6]), [5,6])
  t(list.nth(default=1,[4],5), 1)
  t(list.nth([4,5,6,7],2), 6)
  t(list.last(default=1,[]), 1)
  t(list.last([12,5,8]), 8)
  t(list.length([]), 0)
  t(list.length([1,2,3]), 3)
  t(list.length(["a","b"]), 2)
  t(list.mem(1,[4,5,6]), false)
  t(list.mem(5,[4,5,6]), true)
  t(list.assoc.mem(1,[(4,"bla"),(5,"foo"),(6,"gni")]), false)
  t(list.assoc.mem(5,[(4,"bla"),(5,"foo"),(6,"gni")]), true)
  t(list.map(fun (x) -> 2*x,[1,2,3]), [2,4,6])
  t(list.map(fun (x) -> 2.*x,[1.,2.,3.]), [2.,4.,6.])
  t(list.map.right(fun (x) -> 2*x,[1,2,3]), [2,4,6])
  t(list.mapi(fun (i,x) -> i+x,[1,1,2]), [1,2,4])
  t(list.filter(fun (x) -> x mod 2 == 0,[1,2,3,4,5]), [2,4])
  t(list.remove(1,[3,1,2,1,1]), [3,2,1,1])
  t(list.remove(1,[3,2]), [3,2])
  t(list.append([1,2], [3,4,5]), [1,2,3,4,5])
  t(list.rev([1,2,3]), [3,2,1])
  t(list.assoc(default=0, "", [("a",1),("b",2)]), 0)
  t(list.assoc("b", [("a",1),("b",2)]), 2)
  t(list.assoc.remove("a", [("b",2),("a",1),("a",3)]), [("b",2),("a",3)])
  t(list.assoc.remove.all("c", [("c",1),("b",2),("c",2)]), [("b",2)])
  t(list.assoc.remove.all("c", [("a",1),("b",2)]), [("a",1),("b",2)])
  t(list.for_all(fun (n) -> n > 0, []), true)
  t(list.for_all(fun (n) -> n > 0, [1,2,3]), true)
  t(list.for_all(fun (n) -> n > 0, [1,0,3]), false)
  t(list.exists(fun (n) -> n > 0, [0,2,0]), true)
  t(list.exists(fun (n) -> n > 0, [0,0,0]), false)
  t(list.fold(fun(x,y)->x+y,1,[2,3,4]), 10)
  t(list.fold.right(fun(x,y)->x+y,1,[2,3,4]), 10)
  t(list.prefix(2,[1,2,3]), [1,2])
  t(list.prefix(3,[1]), [1])
  t(list.init(4, fun(x)->2*x), [0,2,4,6])
  t(list.index(fun(x)->x==0,[1,2,0,3]), 2)
  t(list.index(fun(x)->x==0,[1,2,3]), 3)
  t(list.insert(0, 1, []), [1])
  t(list.insert(0, 1, [2, 3, 4]), [1, 2, 3, 4])
  t(list.insert(1, 2, [1, 3, 4]), [1, 2, 3, 4])
  t(list.insert(4, 5, [1, 2, 3, 4]), [1, 2, 3, 4, 5])
  t(list.flatten([[1,2],[3,4]]), [1,2,3,4])
  def even(x) =
    (x/2)*2 == x
  end
  t(list.find(default=42,even,[1,3,5]), 42)
  t(list.find(even,[1,3,4,5,6]), 4)
  t(list.sort.natural(["b","c","a"]), ["a","b","c"])
  l = list.indexed(["a","b","c"])
  t(l[1], "b")
  l = [(1,"a"),(2,"b"),(3,"c")]
  t(l[2],"b")
  t(l[27],"")
  t([1, ...[2,3,4], ...[5,6], 7], [1,2,3,4,5,6,7])

  let [x,y] = [1,2,3]
  t(x, 1)
  t(y, 2)

  let [..._,x,y] = [1,2,3]
  t(x, 2)
  t(y, 3)

  let [x,_,...z] = [1,2,3]
  t(x, 1)
  t(z, [3])

  # Ensure that common methods are kept and others are forgotten
  l = [ "a".{b = true, k = 0} , "b".{b = true, l = 1} ]
  t(list.for_all(fun (x) -> x.b, l), true)
  _ = [ "a".{b = true}, "c" ]

  # There used to be a bug where [x] would loose all its methods.
  x = 1
  let x.foo = "bla"
  list.add(x, [2.{foo = "gni"}])

  try
    ignore(list.insert(23, 2, [1, 2, 3]))
    test.fail()
  catch err : [error.not_found] do
    t(err.kind, "not_found")
    t(err.message, "List should have at least 23 elemments")
  end

  # Test huge lists, see #2162.
  # l = list.init(100000, fun(i) -> i)

  if success() then test.pass() else test.fail() end
end

test.check(f)
