count = ref 1
fail = ref false

def echo(s)
  # system("echo "^quote(s))
  if s != string_of(!count) then
    fail := true
  end
  count := !count + 1
  ()
end

def test(lbl,f)
  if f() then echo(lbl) else echo("fail "^lbl) end
end

test("1",{ 1==1 })
test("2",{ 1+1==2 })
test("3",{ (-1)+2==1 })
test("4",{ (-1)+2 <= 3*2 })
test("5",{ true })
test("6",{ true and true })
test("7",{ 1==1 and 1==1 })
test("8",{ (1==1) and (1==1) })
test("9",{ true and (-1)+2 <= 3*2 })

l = [ ("bla",""), ("bli","x"), ("blo","xx"), ("blu","xxx"), ("dix","10") ]
echo(l["dix"])
test("11",{ 2 == list.length(string.split(separator="",l["blo"])) })

%ifdef foobarbaz
  if = if is not a well-formed expression, and we do not care...
%endif

echo("1#{1+1}")
echo(string_of(int_of_float(float_of_string(default=13.,"blah"))))

f=fun(x)->x
# Checking that the following is not recursive:
f=fun(x)->f(x)
echo(string_of(f(14)))

if !fail then print("TEST FAILED") else print("TEST PASSED") end
