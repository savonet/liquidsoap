%include "../test.liq"

# Hold whatever the current time is.
def always(p) =
  p = time.predicate(p)
  test.equal(p(), true)
end

# Hold at no time.
def never(p) =
  p = time.predicate(p)
  test.equal(p(), false)
end

def f() =
  always(
    "0w or not 0w"
  )
  always(
    "1w or 2w or 3w or 4w or 5w or 6w or 7w"
  )
  always(
    "(1w and 2w) or not 1w or not 2w"
  )
  never(
    "1w and 2w"
  )
  never(
    "0w and not 0w"
  )
  never(
    "(1w or 2w) and (3w or 4w)"
  )
  never(
    "(0w and not 0w) or (1w and 2w)"
  )

  # `and` binds looser than `or`, as in scripts: this is
  # `1w and (2w or not 1w)`.
  never(
    "1w and 2w or not 1w"
  )

  try
    ignore(
      time.predicate(
        "0w garbage"
      )
    )
    test.fail()
  catch _ do
    ()
  end

  test.pass()
end

test.check(f)
