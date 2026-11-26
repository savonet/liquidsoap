%include "../test.liq"
%include "hls_lib.liq"

# Timed ID3 tags stay in the segment of their time behind a video encoder
# with latency: libx264's default lookahead holds video packets well behind
# audio ones and the tags. Titles change every 1.5 s, mid-segment every other
# time. The first segment and the one opened at stop are
# not checked: the latter's replay tag comes after the container closed.
settings.frame.video.width := 320
settings.frame.video.height := 240

dir = file.temp_dir(cleanup=true, "hls-id3-latency")
def tone(n) =
  metadata.map(
    insert_missing=true,
    fun (_) ->
      [
        (
          "title",
          "tone #{n}"
        )
      ],
    once(sine(duration=1.5))
  )
end

audio = source.tracks(sequence(list.init(12, tone)))
s =
  source(
    {
      audio = audio.audio,
      metadata = audio.metadata,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
clock.assign_new(sync="none", [s])

def id3_times(segment) =
  probe =
    process.read(
      "ffprobe -v error -select_streams d -show_entries packet=pts_time -of json \
       #{process.quote(segment)}"
    )
  let json.parse ({packets} : {packets: [{pts_time: string}]}) = probe
  list.map(fun (p) -> float_of_string(p.pts_time), packets)
end

def video_start(segment) =
  list.fold(
    fun (m, p) -> if p.type == "video" and p.pts < m then p.pts else m end,
    infinity,
    hls_test.packets(segment)
  )
end

def check() =
  p = hls_test.playlist("#{dir}/av.m3u8")
  segments = list.map(fun (seg) -> "#{dir}/#{seg.uri}", p.segments)
  n = list.length(segments)
  hls_test.check(
    n >= 4,
    "only #{n} segments listed"
  )
  starts = list.map(video_start, segments)
  list.iteri(
    fun (i, segment) ->
      if
        i > 0 and i < n - 1
      then
        start = list.nth(starts, i)
        stop = i + 1 < n ? list.nth(starts, i + 1) : infinity
        tags = id3_times(segment)
        list.iter(
          fun (t) ->
            hls_test.check(
              t >= start - 0.05 and t < stop,
              "#{segment}: ID3 tag at #{t}s outside its media [#{start}, #{
                stop
              })"
            ),
          tags
        )
      end,
    segments
  )
  hls_test.done(5)
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
        )
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
