%include "../test.liq"

# Video generators declare their video track, so a video-only output can use
# them without crashing on a frame that has no video.

def count_frames(s) =
  n = ref(0)
  s.on_frame(synchronous=true, fun () -> ref.incr(n))
  output.dummy(s)
  n
end

frames =
  [
    ("testsrc", count_frames(video.testsrc())),
    ("board", count_frames(video.board())),
    ("text", count_frames(video.text.native("hello")))
  ]

thread.run(
  delay=2.,
  fun () ->
    begin
      def is_idle(c) =
        let (_, n) = c
        n() == 0
      end
      idle = list.map(fst, list.filter(is_idle, frames))
      if
        idle == []
      then
        test.pass()
      else
        test.fail(
          "no frames from: #{idle}"
        )
      end
    end
)
