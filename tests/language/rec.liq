# Some tests related to recursion and polymorphism.

def f() =
  def id (x) = x end
  _ = id(3)
  _ = id("a")

  def rec recid (x) = x end
  _ = recid(3)
  _ = recid("a")

  def rec nid (n, x) =
    if n == 0 then x else nid(n-1, x) end
  end
  _ = nid(2,3)
  _ = nid(2,"a")

  test.pass()
end

test.check(f)
