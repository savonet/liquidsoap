%include "../test.liq"
%include "hls_lib.liq"

# A run stopped before libx264 produced a packet still writes no timestamp
# below zero, and both variants share their offset: the codecs are flushed
# before the offset is set.
settings.frame.video.width := 320
settings.frame.video.height := 240

dir = file.temp_dir(cleanup=true, "hls-short-stop")
s =
  max_duration(
    0.3,
    source(
      {
        audio = source.tracks(sine()).audio,
        video = source.tracks(video.testsrc.ffmpeg()).video
      }
    )
  )
clock.assign_new(sync="none", [s])

def first(type, packets) =
  list.fold(
    fun (m, p) -> if p.type == type and p.pts < m then p.pts else m end,
    infinity,
    packets
  )
end

def check_variant(name) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  packets =
    list.flatten(
      list.map(fun (seg) -> hls_test.packets("#{dir}/#{seg.uri}"), p.segments)
    )
  list.iter(
    fun (p) ->
      hls_test.check(
        p.dts >= 0. and p.dts < 10. and p.pts >= 0. and p.pts < 10.,
        "#{name}: #{p.type} packet at pts #{p.pts}, dts #{p.dts}"
      ),
    packets
  )
  audio = first("audio", packets)
  video = first("video", packets)
  hls_test.check(
    audio < infinity and video < infinity,
    "#{name}: audio starts at #{audio}, video at #{video}"
  )
  (audio, video)
end

def check() =
  let (audio_no_b, video_no_b) = check_variant("no_b")
  let (audio_b, video_b) = check_variant("b")
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

hls_test.at_first_stop(o, check)
