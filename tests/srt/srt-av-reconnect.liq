# An audio and video input disconnected mid-stream, several times, resumes
# inside a fallback each time (spec/input.md §2).
%include "../test.liq"
%include "helpers.inc"

port = 19570
output.srt(
  host="127.0.0.1",
  port=port,
  polling_delay=0.2,
  %ffmpeg(
    format = "mpegts",
    %audio(codec = "aac"),
    %video(codec = "mpeg2video")
  ),
  mux_video(video=video.testsrc(), sine())
)
i = input.srt(port=port)
connections = ref(0)
i.on_connect(synchronous=true, fun () -> connections := connections() + 1)
selected = ref(0)
f = fallback([i, mux_video(video=video.testsrc(), blank())])
f.on_track(
  synchronous=true,
  fun (_) -> if i.is_ready() then selected := selected() + 1 end
)
output.dummy(f)

def run() =
  for n = 1 to 4 do
    wait_until(
      timeout=60.,
      what="decoded data on connection #{n}",
      {connections() == n and i.is_ready()}
    )
    thread.pause(random.float(min=0.3, max=1.))
    i.disconnect()
    wait_until(
      what="disconnection #{n}",
      {i.sockets() == []}
    )
    i.connect()

  end
  wait_until(
    timeout=60.,
    what="the last connection",
    {connections() == 5 and i.is_ready()}
  )
  test.pass()
end

thread.run(delay=0.1, run)
