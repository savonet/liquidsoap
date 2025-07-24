# Ignored anonymous argument
# f : ('a) -> int = fun (_) -> 123
def f(_) =
  123
end

# Typed anonymous argument
# f : (int) -> int = <fun>
def f((foo:int)) =
  foo
end

# Anonymous argument with default value
# f : (?int) -> int = <fun>
def f(foo=123) =
  foo
end

# Typed ignored anonymous argument
# f : (int) -> int = fun (_) -> 123
def f((_:int)) =
  123
end

# Typed anonymous argument with default value
# f : (?int) -> int = <fun>
def f((foo:int)=123) =
  foo
end

# Ignored anonymous argument with default value
# f : (?int) -> int = fun (_=123) -> 456
def f(_=123) =
  456
end

# Typed ignored anonymous argument with default value
# f : (?int) -> int = fun (_=123) -> 456
def f((_:int)=123) =
  456
end

# Typed named argument
# f : (foo : int) -> int = <fun>
def f(~(foo:int)) =
  foo
end

# Named argument with rename
# f : (foo : 'a) -> 'a = <fun>
def f(~foo:bla) =
  bla
end

# Ignored named argument
# f : (foo : 'a) -> int = fun (~foo=_) -> 123
def f(~foo:_) =
  123
end

# Named argument with default value
# f : (?foo : int) -> int = <fun>
def f(~foo=123) =
  foo
end

# Typed named argument with rename
# f : (foo : int) -> int = <fun>
def f(~foo:(bla:int)) =
  bla
end

# Typed named argument with default value
# f : (?foo : int) -> int = <fun>
def f(~(foo:int)=123) =
  foo
end

# Typed named argument with rename and default value
# f : (?foo : int) -> int = <fun>
def f(~foo:(bla:int)=123) =
  bla
end

# Typed ignored named argument with default value
# f : (?foo : int) -> int = fun (~foo=123) -> 456
def f(~foo:(_:int)=123) =
  456
end

# Ignored argument with default value
# f : (?foo : int) -> int = fun (~foo=123) -> 456
def f(~foo:_=123) =
  456
end
