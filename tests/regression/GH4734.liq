%include "../test.liq"

# A clock conflict raised while building sources at runtime is an error of
# kind "clock": a `try` catching that kind catches it, one filtering on
# another kind does not, error monitors see it, and so do `thread.run` error
# handlers.

settings.init.force_start := true

a = sine()
output.dummy(fallible=true, a)

b = sine()
output.dummy(fallible=true, b)

def combine() =
  output.dummy(fallible=true, switch([({true}, a), ({true}, b)]))
end

monitored = ref([])
error.on_error(fun (e) -> monitored := [...monitored(), e.kind])

def check_try() =
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
end

handled = ref("")

def check() =
  caught = check_try()
  thread.run(on_error=(fun (e) -> handled := error.methods(e).kind), combine)
  thread.run(
    delay=0.5,
    fun () ->
      if
        not caught
      then
        test.fail(
          "the clock conflict was not caught as error.clock"
        )
      elsif
        not list.mem("clock", monitored())
      then
        test.fail(
          "error monitors did not see the clock conflict"
        )
      elsif
        handled() != "clock"
      then
        test.fail(
          "thread.run's on_error got kind \"#{handled()}\""
        )
      else
        test.pass()
      end
  )
end

thread.run(delay=1., check)
