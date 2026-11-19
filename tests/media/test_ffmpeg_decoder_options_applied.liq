%include "../test.liq"

# A decoder option reaches the decoder that decodes (decoder.md §4.4):
# "video.skip_frame"="nokey" decodes key frames only, so the picture freezes
# between them. The same file decoded without it is the reference: no freeze.

fname = argv(default="", 1)

def render(options) =
  output_path = file.temp(cleanup=true, "decoder-options", ".mkv")
  s = once(single("annotate:ffmpeg_options='#{options}':#{fname}"))
  s = max_duration(9., (s : source(video=yuv420p)))
  closed = ref(false)
  output.file(
    fallible=true,
    on_close=fun (_) -> closed := true,
    %ffmpeg(%video(codec = "libx264", preset = "ultrafast")),
    output_path,
    s
  )
  {path = output_path, closed = closed, source = s}
end

skipped = render('"video.skip_frame"="nokey"')
reference = render('"video.flags"="low_delay"')
clock.assign_new(id="test", sync="none", [skipped.source, reference.source])

def freezes(video_path) =
  freezedetect_output =
    process.read(
      "ffmpeg -hide_banner -nostats -i #{process.quote(video_path)} -an -vf \
       freezedetect=n=0.001:d=1 -f null - 2>&1"
    )
  list.length(r/freeze_start/g.split(freezedetect_output)) - 1
end

checked = ref(false)

def check() =
  if
    not checked() and skipped.closed() and reference.closed()
  then
    checked := true
    skipped_freezes = freezes(skipped.path)
    reference_freezes = freezes(reference.path)
    if
      skipped_freezes > 0 and reference_freezes == 0
    then
      test.pass()
    else
      print(
        "Expected freezes with skip_frame only, got #{skipped_freezes} and #{
          reference_freezes
        }"
      )
      test.fail()
    end
  end
end

thread.run(every=0.5, check)
thread.run(delay=60., fun () -> test.fail())
