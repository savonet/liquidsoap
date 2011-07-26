# We test some ground values for json import/export.
success = ref true

def test(d,x) = 
  if of_json(default=d,json_of(x)) == d then
    success := false
  end
end

test(2, 1)
test(3.14, 4.25)
test((1,"foo"), (2,"bar"))
test([("foo",(1,"bar"))], [("gni",(2,"boo"))])
test([(1,[("fr","bar")])], [(2,[("en","foo")])])
test([("ping",())], [("pong",())])
# More.. ?

if !success then 
  print("TEST PASSED")
else
  print("TEST FAILED")
end
