#!../src/liquidsoap ../libs/pervasives.liq

# We test some ground values for json import/export.

%include "test.liq"

success = ref true

def test(d,x) =
  y = of_json(default=d,json_of(x))
  if y == d or y != x then
    print("Failure: #{x} => #{json_of(x)} => #{y}")
    success := false
  end
end

test(2, 1)
test(3.14, 4.25)
test(false, true)
test("abc", "def")
test([1],[1,2,3])
test((1,"foo"), (2,"bar"))
test([("foo",(1,"bar"))], [("gni",(2,"boo"))])
test([(1,[("fr","bar")])], [(2,[("en","foo")])])
# test([("ping",())], [("pong",())])
test([3],[])
test([("x",0)],of_json(default=[("x",0)],"{\"a\" : 4}"))

if !success then
  test.pass()
else
  test.fail()
end
