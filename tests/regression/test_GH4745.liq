# Make sure that fetch method always call fetch function
# even when `prefetch=0`.
def f() =
  s =
    request.dynamic(
      prefetch=0,
      {
        test.pass()
        null
      }
    )

  s.fetch()
end

test.check(f)
