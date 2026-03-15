daytime = time.predicate("9h-21h")

def f() =
  print(daytime())
  test.pass()
end

test.check(f)
