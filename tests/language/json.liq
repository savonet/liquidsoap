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
  y = json.parse(default=d,json.stringify(x))
  if y == d or y != x then
    print("Failure: #{x} => #{json.stringify(x)} => #{y}")
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
  u([("x",0)],json.parse(default=[("x",0)],"{\"a\" : 4}"))

  t(json.stringify(()), '[  ]')
  t(json.stringify("a"), '"a"')
  t(json.stringify("©"), '"©"')
  t(json.stringify('"'), '"\\""')
  t(json.stringify('\\'), '"\\\\"')
  t(json.stringify(infinity), 'null')
  t(json.stringify((0.-infinity)), 'null')
  t(json.stringify(nan), 'null')
  t(json.stringify(json5=true, infinity), 'Infinity')
  t(json.stringify(json5=true, (0.-infinity)), '-Infinity')
  t(json.stringify(json5=true, nan), 'NaN')

  t(json.parse(default=[("","")], '{"a":3}'), [("a","3")])
  t(json.parse(default={x=0, a=""}, '{"a":"z", "x":3}'), {x=3, a="z"})

  j = json()
  j.add("foo", 1)
  j.add("bla", "bar")
  j.add("baz", 3.14)
  j.add("key_with_methods", "value".{method = 123})
  j.add("record", { a = 1, b = "ert"})
  j.remove("foo")
  t(json.stringify(j), '{ "record": { "b": "ert", "a": 1 }, "key_with_methods": "value",\n"bla": "bar", "baz": 3.14\n}')

  if !success then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
