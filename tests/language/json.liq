#!../../src/liquidsoap ../../libs/pervasives.liq

# We test some ground values for json import/export.

%include "test.liq"

success = ref(true)

def t(x,y) =
  if x != y then
    print("Failure: #{x} instead of #{y}")
    success := false
  end
end

def u(d,x) =
  y = of_json(default=d,json_of(x))
  if y == d or y != x then
    print("Failure: #{x} => #{json_of(x)} => #{y}")
    success := false
  end
end

def f() =
  u(2, 1)
  u(3.14, 4.25)
  u(false, true)
  u("abc", "def")
  u([1],[1,2,3])
  u((1,"foo"), (2,"bar"))
  u([("foo",(1,"bar"))], [("gni",(2,"boo"))])
  u([(1,[("fr","bar")])], [(2,[("en","foo")])])
  # u([("ping",())], [("pong",())])
  u([3],[])
  u([("x",0)],of_json(default=[("x",0)],"{\"a\" : 4}"))

  t(of_json(default=[("","")], '{"a":3}'), [("a","3")])
  t(of_json(default={x=0, a=""}, '{"a":"z", "x":3}'), {x=3, a="z"})

  if !success then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
