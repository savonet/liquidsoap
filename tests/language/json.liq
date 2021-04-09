#!../src/liquidsoap ../libs/main.liq

# We test some ground values for json import/export.

%include "../test.liq"

success = ref(true)

def t(d,x) =
  y = of_json(default=d,json_of(x))
  if y == d or y != x then
    print("Failure: #{x} => #{json_of(x)} => #{y}")
    success := false
  end
end

def f() =
  t(2, 1)
  t(3.14, 4.25)
  t(false, true)
  t("abc", "def")
  t([1],[1,2,3])
  t((1,"foo"), (2,"bar"))
  t([("foo",(1,"bar"))], [("gni",(2,"boo"))])
  t([(1,[("fr","bar")])], [(2,[("en","foo")])])
  # t([("ping",())], [("pong",())])
  t([3],[])
  t([("x",0)],of_json(default=[("x",0)],"{\"a\" : 4}"))

  if !success then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
