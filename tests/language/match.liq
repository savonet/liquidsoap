#!../../src/liquidsoap -i ../../libs/stdlib.liq

%include "test.liq"
  
success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

# Two ground types
def f(x) =
  match x
    case x : string do
      ignore (x^"")
      4
    case y : int do
      y+2
  end
end

# Constructors
def g(x) =
  match x
    case `a do "a"
    case `b do "b"
    case `c do "c"
  end
end

# Constructors with a catchall
def h(x) =
  match x
    case `a do "a"
    case `b do "b"
    case `c do "c"
    case _ do "?"
  end
end
  
def run() =
  # Simple test
  x = match 2
    case x do 4
  end

  _ = f("x")
  _ = f(2)

  _ = g(`a)
  _ = g(`b)
  _ = g(`c)

  _ = h(`a)
  _ = h(`b)
  _ = h(`c)
  _ = h("x")

  if !success then test.pass() else test.fail() end
end

test.check(run)
