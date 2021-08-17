#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

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

  t(json_of("a"), '"a"')
  t(json_of("©"), '"©"')
  t(json_of('"'), '"\\""')
  t(json_of('\\'), '"\\\\"')
  t(json_of(infinity), 'null')
  t(json_of((0.-infinity)), 'null')
  t(json_of(nan), 'null')
  t(json_of(json5=true, infinity), 'Infinity')
  t(json_of(json5=true, (0.-infinity)), '-Infinity')
  t(json_of(json5=true, nan), 'NaN')

  t(of_json(default=[("","")], '{"a":3}'), [("a","3")])
  t(of_json(default={x=0, a=""}, '{"a":"z", "x":3}'), {x=3, a="z"})

  j = json()
  j.add("foo", 1)
  j.add("bla", "bar")
  j.add("baz", 3.14)
  j.add("key_with_methods", "value".{method = 123})
  j.add("record", { a = 1, b = "ert"})
  j.remove("foo")
  t(json_of(j), '{ "record": { "b": "ert", "a": 1 }, "key_with_methods": "value",\n"bla": "bar", "baz": 3.14\n}')

  if !success then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
