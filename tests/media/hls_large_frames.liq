%include "../test.liq"
%include "hls_lib.liq"

# segmenting.md §5: with 0.1 s internal frames and re-encoded H.264, every
# EXTINF rounds to at most the target duration and every segment starts with
# a key frame.
settings.frame.duration := 0.1
settings.frame.video.width := 320
settings.frame.video.height := 240
dir = file.temp_dir(cleanup=true, "hls-large-frames")

s =
  source(
    {
      audio = source.tracks(sine()).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=2.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "v",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", preset = "ultrafast", tune = "zerolatency")
        )
      )
    ],
    s
  )

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "v_10.ts"
    then
      o.stop()
    end
)

def check() =
  p = hls_test.playlist("#{dir}/v.m3u8")
  hls_test.check(
    p.target == 2,
    "target duration #{p.target}"
  )
  hls_test.check(
    list.length(p.segments) >= 10,
    "#{list.length(p.segments)} segments"
  )
  durations = list.map(fun (seg) -> seg.duration, p.segments)
  list.iter(
    fun (seg) ->
      begin
        hls_test.check(
          int_of_float(seg.duration + 0.5) <= p.target,
          "#{seg.uri} lasts #{seg.duration}, over the target #{p.target}: #{
            durations
          }"
        )
        video =
          list.filter(
            fun (pk) -> pk.type == "video",
            hls_test.packets("#{dir}/#{seg.uri}")
          )
        hls_test.check(
          video != [] and list.hd(video).key,
          "#{seg.uri} does not start with a key frame"
        )
      end,
    p.segments
  )
  hls_test.done(22)
end

hls_test.at_first_stop(o, check)
