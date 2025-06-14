def f() =
  def a(_, res) =
    res.json({a=null.make()})
  end
  def b(_, res) =
    res.json({b=null.make()})
  end
  harbor.http.register(port=3303, "/a", a)
  harbor.http.register(port=3303, "/b", b)

  test.pass()
end

test.check(f)
