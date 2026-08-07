def f() =
  # Record fields are evaluated when the record is built, whether or not they
  # are ever invoked, and whether or not the record is bound to a name first.
  # Liquidsoap is eager: an optional argument's default is evaluated when the
  # function is defined, for the same reason.
  bound = ref(false)
  r = {a = 1, b = bound := true}
  ignore(r.a)
  test.equal(bound(), true)

  literal = ref(false)
  ignore({a = 1, b = literal := true}.a)
  test.equal(literal(), true)

  default = ref(false)
  def with_default(~x = default := true) =
    ignore(x)
  end
  ignore(with_default)
  test.equal(default(), true)

  test.pass()
end

test.check(f)
