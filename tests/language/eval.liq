#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

count = ref(1)
fail = ref(false)

def echo(s)
  # system("echo "^quote(s))
  if s != string_of(!count) then
    fail := true
  end
  count := !count + 1
  ()
end

def t(lbl,f)
  if f() then echo(lbl) else echo("fail "^lbl) end
end

def f() =
  t("1", { 1==1 })
  t("2", { 1+1==2 })
  t("3", { (-1)+2==1 })
  t("4", { (-1)+2 <= 3*2 })
  t("5", { true })
  t("6", { true and true })
  t("7", { 1==1 and 1==1 })
  t("8", { (1==1) and (1==1) })
  t("9", { true and (-1)+2 <= 3*2 })

  l = [ ("bla",""), ("bli","x"), ("blo","xx"), ("blu","xxx"), ("dix","10") ]
  echo(l["dix"])
  t("11",{ 2 == list.length(string.split(separator="",l["blo"])) })

%ifdef foobarbaz
  if = if is not a well-formed expression, and we do not care...
%endif

  echo("1#{1+1}")
  echo(string_of(int_of_float(float_of_string(default=13.,"blah"))))

  f=fun(x)->x
  # Checking that the following is not recursive:
  f=fun(x)->f(x)
  echo(string_of(f(14)))

  t("15",{ list.remove(2,[2]) == [] })

  t("16", { "bla" == true ? "bla" : "foo" })
  t("17", { "foo" == false ? "bla" : "foo" })

  if !fail then test.fail() else test.pass() end
end

test.check(f)
