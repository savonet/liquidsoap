%include "../test.liq"

# A clock conflict raised while building sources at runtime can be caught as
# an error of kind "clock", and only by a `try` that catches that kind.

settings.init.force_start := true

a = sine()
output.dummy(fallible=true, a)

b = sine()
output.dummy(fallible=true, b)

def combine() =
  output.dummy(fallible=true, switch([({true}, a), ({true}, b)]))
end

def check() =
  passed_through =
    try
      try
        combine()
        false
      catch _ : [error.not_found] do
        false
      end
    catch e : [error.clock] do
      print(
        "Caught: #{e}"
      )
      true
    end

  if
    passed_through
  then
    test.pass()
  else
    test.fail(
      "the clock conflict was not caught as error.clock"
    )
  end
end

thread.run(delay=1., check)
