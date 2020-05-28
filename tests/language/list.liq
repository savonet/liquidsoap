#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"
  
success = ref(true)

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  test(list.hd(default=0,[]), 0)
  test(list.hd(default=0,[5,6]), 5)
  test(list.tl([]), [])
  test(list.tl([4,5,6]), [5,6])
  test(list.nth(default=1,[4],5), 1)
  test(list.nth(default=1,[4,5,6,7],2), 6)
  test(list.last(default=1,[]), 1)
  test(list.last(default=1,[12,5,8]), 8)
  test(list.length([]), 0)
  test(list.length([1,2,3]), 3)
  test(list.length(["a","b"]), 2)
  test(list.mem(1,[4,5,6]), false)
  test(list.mem(5,[4,5,6]), true)
  test(list.map(fun (x) -> 2*x,[1,2,3]), [2,4,6])
  test(list.map(fun (x) -> 2.*x,[1.,2.,3.]), [2.,4.,6.])
  test(list.mapi(fun (i,x) -> i+x,[1,1,2]), [1,2,4])
  test(list.filter(fun (x) -> x mod 2 == 0,[1,2,3,4,5]), [2,4])
  test(list.remove(1,[3,1,2,1,1]), [3,2,1,1])
  test(list.remove(1,[3,2]), [3,2])
  test(list.append([1,2], [3,4,5]), [1,2,3,4,5])
  test(list.rev([1,2,3]), [3,2,1])
  test(list.assoc(default=0, "", [("a",1),("b",2)]), 0)
  test(list.assoc(default=0, "b", [("a",1),("b",2)]), 2)
  test(list.remove_assoc("a", [("b",2),("a",1),("a",3)]), [("b",2),("a",3)])
  test(list.remove_assoc("c", [("a",1),("b",2)]), [("a",1),("b",2)])
  test(list.for_all(fun (n) -> n > 0, []), true)
  test(list.for_all(fun (n) -> n > 0, [1,2,3]), true)
  test(list.for_all(fun (n) -> n > 0, [1,0,3]), false)
  test(list.exists(fun (n) -> n > 0, [0,2,0]), true)
  test(list.exists(fun (n) -> n > 0, [0,0,0]), false)
  test(list.fold(fun(x,y)->x+y,1,[2,3,4]), 10)
  test(list.fold_right(fun(x,y)->x+y,1,[2,3,4]), 10)
  test(list.index(fun(x)->x==0,[1,2,0,3]), 2)
  test(list.index(fun(x)->x==0,[1,2,3]), 3)
  def even(x) =
    (x/2)*2 == x
  end
  test(list.find(default=42,even,[1,3,5]), 42)
  test(list.find(default=42,even,[1,3,4,5,6]), 4)
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
