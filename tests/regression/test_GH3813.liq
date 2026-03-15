def f() =
  r = request.create("annotate:initial_uri=bogus:#{argv(0)}")
  if not request.resolve(r) then test.fail() end

  m = request.metadata(r)
  test.not.equal(m["initial_uri"], "bogus")
  test.pass()
end

test.check(f)
