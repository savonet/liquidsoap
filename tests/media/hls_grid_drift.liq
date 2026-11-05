%include "../test.liq"
%include "hls_lib.liq"

# KC2: AAC-LC and HE-AAC variants of one source on a 2 s grid. An HE-AAC
# frame is longer than an internal frame, so its sum is held to one codec frame.
dir = file.temp_dir(cleanup=true, "hls-grid")

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    segment_duration=2.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "lc",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      ),
      (
        "he",
        %ffmpeg(
          format = "mpegts",
          %audio(
            codec = "libfdk_aac",
            profile = "aac_he",
            samplerate = 44100,
            channels = 2
          )
        )
      )
    ],
    s
  )

internal_frame = settings.frame.duration()
he_frame = 2048. / 44100.

def check() =
  lc = hls_test.playlist("#{dir}/lc.m3u8")
  he = hls_test.playlist("#{dir}/he.m3u8")
  list.iter(
    fun (np) ->
      begin
        let (name, p, tolerance) = np
        first_ten = list.prefix(10, p.segments)
        sum = list.fold(fun (t, seg) -> t + seg.duration, 0., first_ten)
        hls_test.check(
          list.length(first_ten) == 10 and abs(sum - 20.) <= tolerance,
          "#{name}: the first ten EXTINFs sum to #{sum}, expected 20 within #{
            tolerance
          }s: #{list.map(fun (s) -> s.duration, first_ten)}"
        )
      end,
    [("lc", lc, internal_frame), ("he", he, he_frame)]
  )

  hls_test.check(
    list.length(lc.segments) >= 51 and list.length(he.segments) >= 51,
    "fewer than 51 segments: lc #{list.length(lc.segments)}, he #{
      list.length(he.segments)
    }"
  )

  lc_starts = hls_test.starts(lc.segments)
  he_starts = hls_test.starts(he.segments)
  for i = 0 to 50 do
    t = list.nth(lc_starts, i)
    t' = list.nth(he_starts, i)
    hls_test.check(
      abs(t - t') <= he_frame,
      "segment #{i} starts at #{t} in lc and #{t'} in he"
    )

  end

  # A player switching variants aligns them on PTS (encoder.md §8.8).
  lc_pts =
    hls_test.first_pts(
      hls_test.packets("#{dir}/#{list.nth(lc.segments, 50).uri}")
    )
  he_pts =
    hls_test.first_pts(
      hls_test.packets("#{dir}/#{list.nth(he.segments, 50).uri}")
    )
  hls_test.check(
    abs(lc_pts - he_pts) <= he_frame,
    "the first PTS of segment 50 is #{lc_pts} in lc and #{he_pts} in he"
  )
  hls_test.done(54)
end

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if ev.state == "created" and ev.path == "#{dir}/he_53.ts" then o.stop() end
)

hls_test.at_first_stop(o, check)
