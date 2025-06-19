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

  # Ignored anonymous argument
  def f(_) = 123 end
  # f : ('a) -> int = fun (_) -> 123

  # Typed anonymous argument
  def f((foo:int)) = foo end
  # f : (int) -> int = <fun>

  # Anonymous argument with default value
  def f(foo = 123) = foo end
  # f : (?int) -> int = <fun>

  # Typed ignored anonymous argument
  def f((_:int)) = 123 end
  # f : (int) -> int = fun (_) -> 123

  # Typed anonymous argument with default value
  def f((foo:int) = 123) = foo end
  # f : (?int) -> int = <fun>

  # Typed ignored anonymous argument with default value
  def f((_:int) = 123) = 456 end
  # f : (?int) -> int = fun (_=123) -> 456

  # Typed named argument
  def f(~(foo:int)) = foo end
  # f : (foo : int) -> int = <fun>

  # Named argument with rename
  def f(~foo:bla) = bla end
  # f : (foo : 'a) -> 'a = <fun>

  # Ignored named argument
  def f(~foo:_) = 123 end
  # f : (foo : 'a) -> int = fun (~foo=_) -> 123

  # Named argument with default value
  def f(~foo=123) = foo end
  # f : (?foo : int) -> int = <fun>

  # Typed named argument with default value
   def f(~(foo:int)=123) = foo end
  # f : (?foo : int) -> int = <fun>

  # Typed named argument with rename and default value
  def f(~foo:(bla:int)=123) = bla end
  # f : (?foo : int) -> int = <fun>

  # Typed ignored named argument with default value
  def f(~foo:(_:int)=123) = 456 end
  # f : (?foo : int) -> int = fun (~foo=123) -> 456

  # Ignored argument with default value
  def f(~foo:_=123) = 456 end
  # f : (?foo : int) -> int = fun (~foo=123) -> 456

  test.pass()
end

test.check(f)
