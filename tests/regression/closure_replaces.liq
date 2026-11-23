%include "../test.liq"

# A closure redefining a captured variable with `let replaces` reads its
# previous value, so it must capture it.
def make() =
  x = {a = 1}
  fun () ->
    begin
      let replaces x = 3
      (x + 0, x.a)
    end
end

def make_tuple() =
  x = {a = 1}
  y = {b = 2}
  fun () ->
    begin
      let replaces (x, y) = (3, 4)
      (x + y, x.a, y.b)
    end
end

def f() =
  g = make()
  test.equal(g(), (3, 1))
  g = make_tuple()
  test.equal(g(), (7, 1, 2))
  test.pass()
end

test.check(f)
