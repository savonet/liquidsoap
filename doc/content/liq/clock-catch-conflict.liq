#!./liquidsoap
# Two sources, each already streaming through its own output, so each runs in
# its own clock.
a = sine()
output.dummy(a)
b = sine()
output.dummy(b)

# Called later, for instance from a server command: they cannot be combined.
def combine() =
  try
    output.dummy(fallible=true, switch([({true}, a), ({true}, b)]))
  catch e : [error.clock] do
    log.important(
      "Could not combine the sources: #{e.message}"
    )
  end
end
