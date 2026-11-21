%include "../test.liq"

# The `x`/`y` metadata of an image request, e.g. set with `annotate:`, used to
# be dropped: the image was always placed at the top-left corner (#3197).

log.level := 4
settings.frame.video.width := 160
settings.frame.video.height := 120
settings.frame.video.framerate := 10

dir = file.temp_dir("image_annotate_position")
on_cleanup({file.rmdir(dir)})
logo = "#{dir}/logo.png"
ignore(
  process.run(
    "ffmpeg -v error -y -f lavfi -i color=red:s=40x40 -frames:v 1 #{
      process.quote(logo)
    }"
  )
)

# Red: V is high. Blue: V is low.
def red_at(filename, x, y) =
  v =
    process.read(
      "ffprobe -v error -f lavfi #{
        process.quote("movie=#{filename},crop=4:4:#{x}:#{y},signalstats")
      } -show_entries frame_tags=lavfi.signalstats.VAVG -of csv=p=0 \
       -read_intervals %+#1"
    )

  v = string.replace(pattern="[,\\s]", fun (_) -> "", v)
  float_of_string(default=0., v) > 200.
end

pending = ref(3)

def check(name, (img:source), ~logo_at, ~no_logo_at) =
  bg = once(video.fill(color=0x0000ff, blank(duration=1.)))
  s = add([bg, max_duration(1., img)])
  s = source.mux.audio(audio=sine(), s)
  clock.assign_new(sync="none", [s])

  def on_close(filename) =
    let (x, y) = logo_at
    let (x', y') = no_logo_at
    logo = red_at(filename, x, y)
    origin = red_at(filename, x', y')
    print(
      "#{name}: logo #{logo}, origin #{origin}"
    )
    if
      logo and not origin
    then
      pending := pending() - 1
      if pending() == 0 then test.pass() end
    else
      test.fail()
    end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(
      format = "mkv",
      %audio(codec = "aac"),
      %video(codec = "libx264", preset = "ultrafast", crf = 0)
    ),
    file.temp(cleanup=true, "image_annotate_#{name}", ".mkv"),
    s
  )
end

check(
  "metadata",
  image("annotate:x=60,y=40,width=20,height=20:#{logo}"),
  logo_at=(68, 48),
  no_logo_at=(2, 2)
)

# Percentages are relative to the frame size.
check(
  "percent",
  image('annotate:x="50%",y="50%",width=20,height=20:#{logo}'),
  logo_at=(88, 68),
  no_logo_at=(2, 2)
)

# An explicit position overrides the metadata.
check(
  "explicit",
  image(x=100, y=80, "annotate:x=60,y=40,width=20,height=20:#{logo}"),
  logo_at=(108, 88),
  no_logo_at=(68, 48)
)
