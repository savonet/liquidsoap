# Regression test for #5266.
#
# With keyframes further apart than `segment_duration`, the output used to
# split anyway, producing segments that do not start on a keyframe and cannot
# be decoded on their own. `wait_for_keyframe` holds the segment open until the
# encoder offers a split point, and `EXT-X-TARGETDURATION` has to grow to cover
# the segments that result.

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

s = mksafe(sine())
v = video.testsrc()
s = source({audio = source.tracks(s).audio, video = source.tracks(v).video})

clock.assign_new(sync="none", [s])

# One keyframe every 100 frames, i.e. every 4s at 25fps, for 1s segments.
enc =
  %ffmpeg(
    format = "mpegts",
    %audio(codec = "aac", b = "64k"),
    %video(codec = "libx264", g = 100, b = "300k", preset = "ultrafast")
  )

segment_duration = 1.

done = ref(false)

def check(fname) =
  extinf = r/#EXTINF:([0-9.]+),/
  targetdur = r/#EXT-X-TARGETDURATION:([0-9]+)/

  longest = ref(0.)
  target = ref(-1.)

  def scan(line) =
    if
      extinf.test(line)
    then
      d = float_of_string(list.assoc(1, extinf.exec(line)))
      longest := max(longest(), d)
    elsif
      targetdur.test(line)
    then
      target := float_of_string(list.assoc(1, targetdur.exec(line)))
    end
  end

  list.iter(scan, string.split(separator="\n", file.contents(fname)))

  if
    0. < longest() and 0. <= target()
  then
    done := true

    # Segments run to the keyframe interval, 4s, rather than being cut at
    # segment_duration. Without waiting they come out just past 1s.
    if
      longest() < 3.
    then
      print(
        "longest segment is #{longest()}s, expected it to reach the keyframe"
      )
      test.fail()
      # Every segment fits in the announced target duration.
    elsif
      target() < longest()
    then
      print(
        "EXT-X-TARGETDURATION #{target()} is below the longest segment #{
          longest()
        }"
      )
      test.fail()
    else
      test.pass()
    end
  end
end

playlist = path.concat(tmp_dir, "stream.m3u8")

def poll() =
  if not done() and file.exists(playlist) then check(playlist) end
end

thread.run(delay=1., every=0.5, poll)

output.file.hls(
  wait_for_keyframe=true,
  segment_duration=segment_duration,
  segments=10,
  tmp_dir,
  [("stream", enc)],
  s
)

thread.run(delay=60., test.fail)
