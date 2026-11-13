# Script code run by a callback may register callbacks of its own, from the
# operator's events and from libsrt's handshake thread (spec/concurrency.md §2).
%include "../test.liq"
%include "helpers.inc"

port = 19558
registered = ref([])

def note(name) =
  atomic({registered := name::registered()})
end

watched = sine()
output.dummy(watched)

def admit(~hs_version, ~peeraddr, ~streamid, _) =
  ignore(hs_version)
  ignore(peeraddr)
  ignore(streamid)
  watched.on_track(synchronous=true, fun (_) -> ())
  note("listen_callback")
  true
end

listener_input = input.srt(port=port, listen_callback=admit)
output.dummy(fallible=true, listener_input)
caller_output =
  output.srt(port=port, host="127.0.0.1", polling_delay=0.2, %wav, sine())

caller_output.on_connect(
  synchronous=true,
  fun () ->
    begin
      watched.on_track(synchronous=true, fun (_) -> ())
      note("on_connect")
    end
)

def run() =
  wait_until(
    what="both registrations, got #{registered()}",
    {
      list.mem("listen_callback", registered())
      and list.mem("on_connect", registered())
    }
  )
  wait_until(
    what="data on the input",
    {listener_input.is_ready()}
  )
  test.pass()
end

thread.run(delay=0.1, run)
