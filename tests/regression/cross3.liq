#!/usr/bin/env -S dune exec ../../src/bin/liquidsoap.exe -- --no-stdlib -i

# See #2080. Simplified version.

def f() =
  def f(a)
    ignore([{foo = "aabb"}, a])
  end

  def g((f : (unit) -> unit))
    f(())
  end

  g(f)

  test.pass()
end

test.check(f)
