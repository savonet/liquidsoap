#!/usr/bin/env -S dune exec ../../src/bin/liquidsoap.exe -- --no-stdlib -i

# See #2080. Simplified version.

s = sine()

def once((s:source))
  sine()
end

# Here, because of once, we infer that f should take as argument a source with
# many methods (id, seek, etc.). However, we use it with cross below, which
# takes a function operating on undecorated sources.
def f(a)
  ignore([a, once(sine())])
end

def cross((f : (source) -> unit), s)
  s
end

s = cross(f, s)
