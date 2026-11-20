%include "../test.liq"
%include "hls_lib.liq"

# encoder.md §3: a raw track with no framerate sizes its HLS key frame
# interval from the source's rate. A 30000/1001 input with 2 s segments
# gets a key frame every 60 frames, not every 50 (2 s at the internal 25 fps).
fname = argv(default="", 1)
dir = file.temp_dir(cleanup=true, "hls-raw-key-frames")

s = (input.ffmpeg(self_sync=false, fname) : source(video=ffmpeg.video.raw))

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
          %video.raw(codec = "libx264", preset = "ultrafast")
        )
      )
    ],
    s
  )

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "v_2.ts"
    then
      o.stop()
    end
)

def check() =
  video =
    list.filter(
      fun (pk) -> pk.type == "video",
      hls_test.packets("#{dir}/v_1.ts")
    )
  frames = list.length(video)
  keys = list.length(list.filter(fun (pk) -> pk.key, video))
  hls_test.check(
    frames == 60,
    "v_1.ts has #{frames} video frames, expected 60"
  )
  hls_test.check(
    keys == 1,
    "v_1.ts has #{keys} key frames, expected 1"
  )
  hls_test.done(2)
end

hls_test.at_first_stop(o, check)
