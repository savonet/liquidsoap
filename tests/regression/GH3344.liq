%include "../test.liq"

# Video generators declare their video track, so a video-only output can use
# them without crashing on a frame that has no video.

frames = ref(0)

list.iter(
  fun (s) ->
    begin
      s.on_frame(synchronous=true, fun () -> ref.incr(frames))
      output.dummy(s)
    end,
  [video.testsrc(), video.board(), video.text("hello")]
)

thread.run(
  delay=2.,
  fun () ->
    if
      frames() > 0
    then
      test.pass()
    else
      test.fail(
        "no frames"
      )
    end
)
