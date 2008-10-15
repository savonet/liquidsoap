set("log.file",false)

echo = fun (x) -> system("echo "^quote(x))

def test(lbl,f)
  if f() then echo(lbl) else system("echo fail "^lbl) end
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
print(f(14))
