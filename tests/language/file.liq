def f() =
  try
    ignore(file.read("mqlskjdfdjnsi"))
    test.fail()
  catch e : [error.file] do
    test.pass()
  end
end

test.check(f)
