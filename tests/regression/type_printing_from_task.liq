# Rendering a type reaches for a formatter, and a task does not necessarily run
# where the last one did.
def f() =
  try
    let json.parse (val : {x: int}) = "[]"
    print(val.x)
  catch err : [error.json] do
    test.equal(
      err.message,
      "Parsing error: json value cannot be parsed as type {x : int}"
    )
  end

  test.pass()
end

test.check(f)
