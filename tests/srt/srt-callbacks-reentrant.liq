# Synchronous callbacks may call the methods of their own operator, in both
# roles and both kinds, and shutdown still completes (spec/concurrency.md §2).
%include "../test.liq"
%include "helpers.inc"

calls = ref([])

def touch(name, x) =
  ignore(x.sockets())
  ignore(x.is_started())
  ignore(x.is_ready())
  x.connect()
  atomic({calls := name::calls()})
end

def watch(label, x) =
  x.on_connect(
    synchronous=true,
    fun () ->
      touch(
        "#{label} on_connect",
        x
      )
  )
  x.on_disconnect(
    synchronous=true,
    fun () ->
      touch(
        "#{label} on_disconnect",
        x
      )
  )
  x.on_socket(
    synchronous=true,
    fun (~mode, _) ->
      touch(
        "#{label} on_socket #{mode}",
        x
      )
  )
end

caller_output =
  output.srt(port=19522, host="127.0.0.1", polling_delay=0.2, %wav, sine())
listener_input = input.srt(port=19522)
output.dummy(fallible=true, listener_input)
listener_output = output.srt(mode="listener", port=19524, %wav, sine())
caller_input =
  input.srt(mode="caller", host="127.0.0.1", port=19524, polling_delay=0.2)
output.dummy(fallible=true, caller_input)
watch(
  "caller output",
  caller_output
)
watch(
  "listener input",
  listener_input
)
watch(
  "listener output",
  listener_output
)
watch(
  "caller input",
  caller_input
)

def fired(name) =
  list.mem(name, calls())
end

def run() =
  wait_until(
    what="data on both inputs",
    {listener_input.is_ready() and caller_input.is_ready()}
  )
  listener_input.disconnect()
  caller_input.disconnect()
  wait_until(
    what="every disconnection",
    {
      fired(
        "caller output on_disconnect"
      )
      and fired(
        "listener input on_disconnect"
      )
      and fired(
        "listener output on_disconnect"
      )
      and fired(
        "caller input on_disconnect"
      )
    }
  )
  list.iter(
    fun (name) ->
      if
        not fired(name)
      then
        test.fail(
          "#{name} never fired"
        )
      end,
    [
      "caller output on_connect",
      "caller output on_socket connect",
      "listener input on_socket listen",
      "listener input on_socket incoming",
      "listener input on_connect",
      "listener output on_socket listen",
      "caller input on_socket connect",
      "caller input on_connect"
    ]
  )
  test.pass()
end

thread.run(delay=0.1, run)
