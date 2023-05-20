def f() =
  x = false

  ignore(x)

#
  x = true

  if x then test.pass() else test.fail () end
end

test.check(f)
