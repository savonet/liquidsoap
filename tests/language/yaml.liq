def f() =
  j = json.object()
  j.add("foo", 1)
  j.add("bla", "bar")
  j.add("baz", 3.14)
  j.add("key_with_methods", "value".{method = 123})
  j.add("record", {a = 1, b = "ert"})
  j.remove("foo")
  j = yaml.stringify(j)
  test.equal(j, 'record:
  a: 1
  b: ert
key_with_methods: value
bla: bar
baz: 3.14
')

  let yaml.parse (x :
    {
      bla: string,
      baz: float,
      key_with_methods: string,
      record: {a: float, b: string}
    }
  ) = j

  test.equal(
    x,
    {
      bla = "bar",
      baz = 3.14,
      key_with_methods = "value",
      record = {a = 1., b = "ert"}
    }
  )

  # Integers parse as int, and still widen when the type asks for a float.
  let yaml.parse (i : {int_field: int, float_field: float}) =
    "int_field: 42\nfloat_field: 42\n"

  test.equal(i, {int_field = 42, float_field = 42.})

  # Wider than OCaml's native int: degrades to float instead of wrapping.
  let yaml.parse (w : {wide: float}) =
    "wide: 9223372036854775807\n"

  test.equal(w, {wide = 9223372036854775807.})

  test.pass()
end

test.check(f)
