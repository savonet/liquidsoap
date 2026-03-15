def f() =
  def a(_, res) =
    try
      res.json(
        json.value(
          "A string"
        )
      )
    catch _ do
      res.json(json.value(1234))
    end
  end
  harbor.http.register(port=3303, "/a", a)

  test.pass()
end

test.check(f)
