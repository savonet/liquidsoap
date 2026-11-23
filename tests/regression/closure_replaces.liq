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

def f() =
  g = make()
  test.equal(g(), (3, 1))
  test.pass()
end

test.check(f)
