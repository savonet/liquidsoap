%include "../test.liq"

# video.tile used the first track's image, unscaled, as the canvas the other
# tiles were added to, so the top-left tile showed the top-left part of the full
# frame instead of the whole first image scaled down (#3606). It also failed on
# video-only sources, which it now takes: it only tiles video.

log.level := 4
settings.frame.video.width := 160
settings.frame.video.height := 120
settings.frame.video.framerate := 10

dir = file.temp_dir("video_tile")
on_cleanup({file.rmdir(dir)})
marked = "#{dir}/marked.png"

# Red, with a white square in its bottom-right corner.
ignore(
  process.run(
    "ffmpeg -v error -y -f lavfi -i color=red:s=160x120 -vf \
     drawbox=x=130:y=90:w=30:h=30:color=white:t=fill -frames:v 1 #{
      process.quote(marked)
    }"
  )
)

def tile(img) =
  max_duration(1., img)
end

s =
  video.tile(
    [
      tile(image(marked)),
      tile(video.fill(color=0x00ff00, blank())),
      tile(video.fill(color=0x0000ff, blank())),
      tile(video.fill(color=0x00ffff, blank()))
    ]
  )

s = source.mux.audio(audio=sine(), s)
clock.assign_new(sync="none", [s])

# Luma of a 4x4 block at (x, y) in the first frame.
def luma_at(filename, x, y) =
  v =
    process.read(
      "ffprobe -v error -f lavfi #{
        process.quote("movie=#{filename},crop=4:4:#{x}:#{y},signalstats")
      } -show_entries frame_tags=lavfi.signalstats.YAVG -of csv=p=0 \
       -read_intervals %+#1"
    )

  float_of_string(
    default=0.,
    string.replace(pattern="[,\\s]", fun (_) -> "", v)
  )
end

def on_close(filename) =
  # Scaled into the top-left 80x60 tile, the white square is near (75, 55).
  # Unscaled, that point is red.
  corner = luma_at(filename, 72, 52)
  print(
    "luma where the scaled square should be: #{corner}"
  )
  if corner > 200. then test.pass() else test.fail() end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    format = "mkv",
    %audio(codec = "aac"),
    %video(codec = "libx264", preset = "ultrafast", crf = 0)
  ),
  file.temp(cleanup=true, "video_tile", ".mkv"),
  s
)
