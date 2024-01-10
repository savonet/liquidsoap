def f() =
  def f(~(foo:int), ~(bar:string)="123", (x:source), (y:int)=123) =
    ()
  end

  # Arguments with same name
  def f(x, x) =
    x
  end

  test.equals(f(2, 3), 3)

  # Free variables with same name
  x = 4
  x = 5

  def f() =
    x
  end

  test.equals(f(), 5)

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
  test.pass()
end

test.check(f)
