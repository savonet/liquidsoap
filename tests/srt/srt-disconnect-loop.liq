# Disconnecting a streaming input from a script thread, in a loop, never
# crashes, and every disconnection ends a track (spec/connection.md §2).
%include "../test.liq"
%include "helpers.inc"

port = 19590
output.srt(
  host="127.0.0.1",
  port=port,
  polling_delay=0.1,
  %ffmpeg(format = "mpegts", %audio(codec = "aac")),
  sine()
)
i = input.srt(port=port)
connections = ref(0)
i.on_connect(synchronous=true, fun () -> connections := connections() + 1)
tracks = ref(0)
i.on_track(synchronous=true, fun (_) -> tracks := tracks() + 1)
output.dummy(fallible=true, i)

def run() =
  for n = 1 to 30 do
    wait_until(
      what="connection #{n}",
      {connections() == n}
    )
    wait_until(
      timeout=30.,
      what="track #{n}",
      {tracks() >= n}
    )
    thread.pause(random.float(min=0., max=0.2))
    i.disconnect()
    i.connect()

  end
  wait_until(
    what="the last connection",
    {connections() == 31}
  )
  wait_until(
    timeout=30.,
    what="the last track",
    {tracks() >= 31}
  )
  test.pass()
end

thread.run(delay=0.1, run)
