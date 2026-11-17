%include "../test.liq"

# files.md §2: `start_time` is the wall-clock time the segment was opened, not
# the time it is named (issue #5645).
dir = file.temp_dir("hls_segment_start_time")
on_cleanup({file.rmdir(dir)})

last_start = ref(0.)
checked = ref(0)

def segment_name(segment) =
  if
    not test.done() and segment.extname == "mp3"
  then
    elapsed = time() - segment.start_time
    if
      elapsed < 0.8 * segment.duration
    then
      test.fail(
        "segment #{segment.position}: start_time #{segment.start_time} is only #{
          elapsed
        }s before naming, duration #{segment.duration}s"
      )
    end
    if
      segment.start_time <= last_start()
    then
      test.fail(
        "segment #{segment.position}: start_time #{segment.start_time} not after \
         #{last_start()}"
      )
    end
    last_start := segment.start_time
    checked := checked() + 1
    if
      checked() == 3
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
  sine()
)
