%include "../test.liq"
%include "hls_lib.liq"

# KC22: two files whose H.264 headers differ, copied into an fMP4 variant
# beside an encoded audio-only variant.
settings.ffmpeg.content.copy.relaxed_compatibility_check := true

first = argv(1)
second = argv(2)
dir = file.temp_dir(cleanup=true, "hls-restart")

s = sequence([once(single(first)), once(single(second))])
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=2.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      ("video", %ffmpeg(format = "mp4", %audio.drop, %video.copy)),
      (
        "audio",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video.drop
        )
      )
    ],
    s
  )

# The cut must land where the source switches files, wherever that is.
switch_time = ref(-1.)
s.on_track(
  synchronous=true,
  fun (_) -> if s.time() > 0. then switch_time := s.time() end
)
video_frame = 1. / 25.
tolerance = video_frame + 0.001

def discontinuities(p) =
  list.fold(
    fun (l, i) -> list.nth(p.segments, i).discontinuous ? [...l, i] : (l),
    [],
    list.init(list.length(p.segments), fun (i) -> i)
  )
end

def media_packets(seg, kind) =
  init_path = seg.map == "" ? "" : "#{dir}/#{seg.map}"
  list.filter(
    fun (p) -> p.type == kind,
    hls_test.packets(init=init_path, "#{dir}/#{seg.uri}")
  )
end

def check_variant(name, p, kind, j) =
  starts = hls_test.starts(p.segments)
  t = list.nth(starts, j)
  hls_test.check(
    switch_time() > 0. and abs(t - switch_time()) <= tolerance,
    "#{name}: the discontinuous segment starts at #{t}, the switch is at #{
      switch_time()
    }"
  )

  before = media_packets(list.nth(p.segments, j - 1), kind)
  after = media_packets(list.nth(p.segments, j), kind)
  last_dts =
    list.fold(fun (m, p) -> p.dts > m ? p.dts : (m), 0. - infinity, before)
  first_dts = list.fold(fun (m, p) -> p.dts < m ? p.dts : (m), infinity, after)
  hls_test.check(
    after != [] and first_dts > last_dts,
    "#{name}: #{kind} DTS go from #{last_dts} to #{first_dts} across the switch"
  )
  hls_test.check(
    after != [] and list.hd(after).key,
    "#{name}: the segment after the switch does not start with a key packet"
  )

  # Boundaries after the switch, but the last segment, closed at the stop.
  list.iteri(
    fun (i, start) ->
      if
        i > j and i < list.length(starts) - 1
      then
        k = float(int_of_float(start / 2. + 0.5)) * 2.
        hls_test.check(
          abs(start - k) <= tolerance,
          "#{name}: segment #{i} starts at #{start}, off the 2 s grid"
        )
      end,
    starts
  )
end

def check() =
  video_pl = hls_test.playlist("#{dir}/video.m3u8")
  audio_pl = hls_test.playlist("#{dir}/audio.m3u8")
  dv = discontinuities(video_pl)
  da = discontinuities(audio_pl)
  hls_test.check(
    list.length(dv) == 1 and list.length(da) == 1,
    "discontinuous segments: video #{dv}, audio #{da}, expected one in each"
  )
  hls_test.check(
    video_pl.discontinuity_sequence == audio_pl.discontinuity_sequence,
    "discontinuity sequence: video #{video_pl.discontinuity_sequence}, audio #{
      audio_pl.discontinuity_sequence
    }"
  )
  j = list.hd(dv)
  hls_test.check(
    j > 0,
    "the first segment is the discontinuous one"
  )
  map_before = list.nth(video_pl.segments, j - 1).map
  map_after = list.nth(video_pl.segments, j).map
  hls_test.check(
    map_after != "" and map_after != map_before,
    "video: EXT-X-MAP #{map_before} before the switch and #{map_after} after"
  )
  hls_test.check(
    list.exists(
      fun (l) -> l == "#EXT-X-MAP:URI=\"#{map_after}\"",
      video_pl.lines
    ),
    "video: no EXT-X-MAP line names #{map_after}"
  )
  check_variant("video", video_pl, "video", j)
  check_variant("audio", audio_pl, "audio", list.hd(da))
  hls_test.done(14)
end

hls_test.at_first_stop(o, check)
