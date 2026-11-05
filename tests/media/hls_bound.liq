%include "../test.liq"
%include "hls_lib.liq"

# KC1: a video with key frames every 4 s, in 1 s segments, re-encoded with the
# same key frame interval then copied.
fixture = argv(1)
log_path = file.temp("hls-bound", ".log")
log.file.path := log_path
log.file := true

encoded_dir = file.temp_dir(cleanup=true, "hls-bound-encoded")
copied_dir = file.temp_dir(cleanup=true, "hls-bound-copied")

encoded_source = once(single(fixture))
clock.assign_new(sync="none", [encoded_source])
encoded =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    encoded_dir,
    [
      (
        "encoded",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", g = 100, sc_threshold = 0)
        )
      )
    ],
    encoded_source
  )

copied_source = once(single(fixture))
clock.assign_new(sync="none", [copied_source])
copied =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    copied_dir,
    [("copied", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))],
    copied_source
  )

checked = ref(0)

def finish() =
  ref.incr(checked)
  if checked() == 2 then hls_test.done(30) end
end

def video_key(dir, seg) =
  firsts = hls_test.first_packets(hls_test.packets("#{dir}/#{seg.uri}"))
  list.exists(fun (p) -> p.type == "video" and p.key, firsts)
end

def check_encoded() =
  p = hls_test.playlist("#{encoded_dir}/encoded.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 15,
    "encoded: only #{n} segments"
  )
  list.iter(
    fun (seg) ->
      begin
        hls_test.check(
          int_of_float(seg.duration + 0.5) <= p.target,
          "encoded: #{seg.uri} EXTINF #{seg.duration} rounds above #{p.target}"
        )
        hls_test.check(
          video_key(encoded_dir, seg),
          "encoded: #{seg.uri} does not start with a video key frame"
        )
      end,
    p.segments
  )
  finish()
end

def check_copied() =
  p = hls_test.playlist("#{copied_dir}/copied.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 12,
    "copied: only #{n} segments"
  )
  hls_test.check(
    not p.independent,
    "copied: EXT-X-INDEPENDENT-SEGMENTS announced with forced cuts"
  )
  not_key = ref(0)
  list.iteri(
    fun (i, seg) ->
      begin
        hls_test.check(
          int_of_float(seg.duration + 0.5) <= p.target,
          "copied: #{seg.uri} EXTINF #{seg.duration} rounds above #{p.target}"
        )
        if i > 0 and not video_key(copied_dir, seg) then ref.incr(not_key) end
      end,
    p.segments
  )
  warnings =
    list.length(
      hls_test.log_lines(
        log_path,
        "Variant copied: forced split"
      )
    )
  hls_test.check(
    not_key() > 0 and warnings >= not_key(),
    "copied: #{not_key()} segments start without a key frame, #{warnings} forced \
     cuts logged"
  )
  finish()
end

hls_test.at_first_stop(encoded, check_encoded)
hls_test.at_first_stop(copied, check_copied)
