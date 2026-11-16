#!./liquidsoap
a = sine()
b = sine()
def combine() =
  try
    output.dummy(fallible=true, switch([({true}, a), ({true}, b)]))
  catch e : [error.clock] do
    log.important(
      "Could not combine the sources: #{e.message}"
    )
  end
end
