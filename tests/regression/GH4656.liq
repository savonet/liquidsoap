%include "../test.liq"

# OCaml exceptions raised by builtins, here an invalid operator parameter, are
# caught by scripts with their kind: in `try`, by `thread.run` error handlers
# and by error monitors, once per error. A `try` filtering on another kind lets
# them through.

def bad() =
  ignore(filter.rc(mode="foo", frequency=100., sine()))
end

monitored = ref([])
error.on_error(fun (e) -> monitored := [...monitored(), e.kind])

def caught_kind() =
  try
    try
      bad()
      ""
    catch _ : [error.not_found] do
      "not_found"
    end
  catch e : [error.invalid] do
    if
      r/Invalid_value/.test(e.origin ?? "")
    then
      e.kind
    else
      "no origin: #{e.origin ?? "none"}"
    end
  end
end

top_level = caught_kind()
in_thread = ref("")
handled = ref("")

thread.run(delay=0.1, {in_thread := caught_kind()})
thread.run(
  delay=0.2,
  on_error=(fun (e) -> handled := error.methods(e).kind),
  bad
)

thread.run(
  delay=1.,
  fun () ->
    if
      top_level != "invalid"
    then
      test.fail(
        "top-level try got \"#{top_level}\""
      )
    elsif
      in_thread() != "invalid"
    then
      test.fail(
        "try in a thread got \"#{in_thread()}\""
      )
    elsif
      handled() != "invalid"
    then
      test.fail(
        "thread.run's on_error got \"#{handled()}\""
      )
    elsif
      list.length(list.filter(fun (k) -> k == "invalid", monitored())) != 3
    then
      test.fail(
        "error monitors saw: #{monitored()}"
      )
    else
      test.pass()
    end
)
