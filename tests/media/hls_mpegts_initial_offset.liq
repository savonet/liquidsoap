%include "../test.liq"
%include "hls_lib.liq"

# No timestamp reaches the mpegts muxer below zero: the AAC priming and the
# B-frame reordering delay are absorbed by one offset shared by the variants.
# A negative timestamp wraps to 2^33, and the first segment reads as 26 hours
# long (#5726).
settings.frame.video.width := 320
settings.frame.video.height := 240

dir = file.temp_dir(cleanup=true, "hls-initial-offset")
s =
  source(
    {
      audio = source.tracks(sine()).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
clock.assign_new(sync="none", [s])

def first(type, packets) =
  list.fold(
    fun (m, p) -> if p.type == type and p.pts < m then p.pts else m end,
    infinity,
    packets
  )
end

def check_first_segment(name) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  seg = list.hd(p.segments)
  packets = hls_test.packets("#{dir}/#{seg.uri}")
  hls_test.check(
    list.length(packets) > 0,
    "#{seg.uri}: no packets"
  )
  list.iter(
    fun (p) ->
      hls_test.check(
        p.dts >= 0. and p.dts < 10. and p.pts >= 0. and p.pts < 10.,
        "#{seg.uri}: #{p.type} packet at pts #{p.pts}, dts #{p.dts}"
      ),
    packets
  )
  audio = first("audio", packets)
  video = first("video", packets)
  hls_test.check(
    audio < infinity and video < infinity,
    "#{seg.uri}: audio starts at #{audio}, video at #{video}"
  )
  (audio, video)
end

def check() =
  let (audio_no_b, video_no_b) = check_first_segment("no_b")
  let (audio_b, video_b) = check_first_segment("b")
  hls_test.check(
    audio_no_b == audio_b and video_no_b == video_b,
    "variants start at different timestamps: audio #{audio_no_b} / #{audio_b}, \
     video #{video_no_b} / #{video_b}"
  )
  hls_test.done(10)
end

o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "no_b",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac"),
          %video(codec = "libx264", x264opts = "bframes=0")
        ).{id3 = false}
      ),
      (
        "b",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac"),
          %video(codec = "libx264", x264opts = "bframes=3")
        ).{id3 = false}
      )
    ],
    s
  )

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "b_3.ts"
    then
      o.stop()
    end
)

hls_test.at_first_stop(o, check)
