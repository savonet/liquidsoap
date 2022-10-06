#!/usr/bin/env -S dune exec ../../src/bin/liquidsoap.exe -- --no-stdlib -i

# See #2080. Simplified version. Here, "float" stands for "source".

# Our main source.
s = 3.

def once((s:float))
  s.{id = "bla"}
end

# Here, because of once, we infer that f should take as argument a source with
# many methods (id, etc.). However, we use it with cross below, which takes a
# function operating on undecorated sources.
def f(a)
  ignore([once(2.), a])
end

def cross((f : (float) -> unit), s)
  ignore(f)
  s
end

s = cross(f, s)

ignore(s)
