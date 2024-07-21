def f() =
  def f(~(foo:int), ~(bar:string)="123", (x:source), (y:int)=123) =
    ()
  end

  # Arguments with same name
  def f(x, x) =
    x
  end

  test.equal(f(2, 3), 3)

  # Free variables with same name
  x = 4
  x = 5

  def f() =
    x
  end

  test.equal(f(), 5)

  def recf() =
    ()
  end

  recf()

  def replacesf() =
    ()
  end

  replacesf()
  let replacesg = ()
  x = replacesg
  let evalf = ()
  x = evalf

  # Function application generalization
  def f() =
    fun (_) -> 1
  end
  fn = f()
  ignore(fn(1))
  ignore(fn("bla"))

  def f() =
    fun (x) -> {foo=x}
  end
  fn = f()
  test.equal(fn(1), {foo=1})
  test.equal(fn("aabb"), {foo="aabb"})

  def f() =
    fun (x) -> {foo=x.gni}
  end
  fn = f()
  test.equal(fn({gni=1}), {foo=1})
  test.equal(fn({gni="aabb"}), {foo="aabb"})

  test.pass()
end

test.check(f)
