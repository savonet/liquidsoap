%include "../test.liq"
%include "hls_lib.liq"

# Audio and video of each segment start together, whatever the video
# encoder's latency: libx264's default lookahead and B-frames hold video
# packets well behind audio ones (#5719).
settings.frame.video.width := 320
settings.frame.video.height := 240

dir = file.temp_dir(cleanup=true, "hls-av-start")
s =
  source(
    {
      audio = source.tracks(sine()).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
clock.assign_new(sync="none", [s])

def min_pts(type, packets) =
  list.fold(
    fun (m, p) -> if p.type == type and p.pts < m then p.pts else m end,
    infinity,
    packets
  )
end

def check() =
  p = hls_test.playlist("#{dir}/av.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 4,
    "only #{n} segments listed"
  )
  list.iter(
    fun (seg) ->
      begin
        packets = hls_test.packets("#{dir}/#{seg.uri}")
        gap = min_pts("audio", packets) - min_pts("video", packets)
        hls_test.check(
          abs(gap) < 0.1,
          "#{seg.uri}: audio starts #{gap}s after video"
        )
      end,
    list.tl(p.segments)
  )
  hls_test.done(4)
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
        "av",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac"),
          %video(codec = "libx264")
        ).{id3 = false}
      )
    ],
    s
  )

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "av_6.ts"
    then
      o.stop()
    end
)

hls_test.at_first_stop(o, check)
