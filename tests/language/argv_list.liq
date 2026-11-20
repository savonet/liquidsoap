%include "../test.liq"

# argv.list() holds the script name followed by the parameters, so that its
# i-th element is argv(i) (#3409).

def f() =
  l = argv.list()
  print(
    "argv.list() = #{l}"
  )
  n = list.length(l)
  consistent =
    list.for_all(
      fun (i) -> list.nth(l, i) == argv(i),
      list.init(n, fun (i) -> i)
    )

  if
    n >= 1 and consistent and argv(default="none", n) == "none"
  then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
