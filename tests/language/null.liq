def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(null() ?? "bla", "bla")
  t(null("foo") ?? "bla", "foo")
  t(null.case(null(),{true},fun (_) -> false),true)
  t(null.case("x",{true}, fun(_) -> false),false)
  t(null.get("x"), "x")
  t(null.get(default="x", "y"), "y")
  t(null.get(default="x", null()), "x")
  t(null.find(fun (x) -> if x mod 2 == 0 then 2*x else null() end, [1,3,2,5]), 4)

  def f((x:int?)) =
    if true then
      1
    else
      x
    end
  end

  def g() =
    if true then
      null()
    else
     (1:int?)
    end
  end

  # Subtyping
  def f() = if true then null() else 1 end end
  def f() = if true then 1 else null() end end
  _ = [null(), 3]
  _ = [3, null()]

  test.pass()
end

test.check(f)
