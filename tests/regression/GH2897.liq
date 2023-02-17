def f() =
  def a()
    def next()
      error.raise(error.not_found, "test")
    end
    request.dynamic(next)
  end
  output.dummy(fallible = true, a())

  def a()
    def next()
      error.raise(error.not_found, "test")
    end
    native.request.dynamic(next)
  end
  output.dummy(fallible = true, a())

  thread.run(delay=1.,test.pass)
end

test.check(f)
