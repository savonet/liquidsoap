%include "../test.liq"

# files.md §2: `start_time` is the segment's start in media time since the run
# began, so each segment starts where the previous one ends (issue #5645).
dir = file.temp_dir("hls_segment_start_time")
on_cleanup({file.rmdir(dir)})

s = sine()
clock.assign_new(sync="none", [s])

next_start = ref(0.)
checked = ref(0)

def segment_name(segment) =
  if
    not test.done() and segment.extname == "mp3"
  then
    if
      abs(segment.start_time - next_start()) > 0.001
    then
      test.fail(
        "segment #{segment.position}: start_time #{segment.start_time}, expected \
         #{next_start()}"
      )
    end
    next_start := segment.start_time + segment.duration
    checked := checked() + 1
    if
      checked() == 5
    then
      print(
        "#{checked()} segment start times checked"
      )
      test.pass()
    end
  end
  "#{segment.stream_name}_#{segment.position}.#{segment.extname}"
end

output.file.hls(
  segment_duration=1.,
  segment_name=segment_name,
  dir,
  [("a", %mp3)],
  s
)
