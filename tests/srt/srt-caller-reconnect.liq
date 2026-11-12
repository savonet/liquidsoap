# A caller output that reconnects gives the new peer a stream from its start:
# the input decodes each connection as a new track (spec/output.md §3, §4).
%include "../test.liq"
%include "helpers.inc"

port = 19510
i = input.srt(port=port)
connections = ref(0)
tracks = ref(0)
i.on_connect(synchronous=true, fun () -> connections := connections() + 1)
i.on_track(synchronous=true, fun (_) -> tracks := tracks() + 1)
output.dummy(fallible=true, i)
output.srt(port=port, polling_delay=0.5, %ogg(%vorbis), sine())

def connected_and_decoding(n) =
  wait_until(
    what="connection #{n}",
    {connections() == n}
  )
  wait_until(
    timeout=30.,
    what="track #{n}",
    {tracks() >= n}
  )
end

def run() =
  connected_and_decoding(1)
  for n = 2 to 3 do
    i.disconnect()
    wait_until(
      what="the input to release",
      {i.sockets() == [] and not i.is_ready()}
    )
    i.connect()
    connected_and_decoding(n)

  end
  test.pass()
end

thread.run(delay=0.1, run)
