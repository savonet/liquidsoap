log.level.set(4)

settings.decoder.ffmpeg.codecs.vp9.set("libvpx-vp9")

fname = argv(default="", 1)
out = file.temp(cleanup=true, "vp9-alpha", ".webm")

s = max_duration(1., single(fname))

clock.assign_new(id='test_clock', sync='none', [s])

done = ref(false)

def on_close(filename) =
  if
    not done()
  then
    done := true

    json =
      process.read(
        "ffprobe -c:v libvpx-vp9 -v error -select_streams v:0 -show_entries \
         stream=pix_fmt:format=duration -of json #{process.quote(filename)}"
      )

    let json.parse (parsed :
      {streams: [{pix_fmt: string?}], format: {duration: string}}
    ) = json

    pix_fmt = null.get(default="", (list.hd(parsed.streams)).pix_fmt)
    duration = float_of_string(default=0., parsed.format.duration)

    if
      pix_fmt == "yuva420p" and duration > 0.
    then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    %video(
      codec = "libvpx-vp9",
      deadline = "realtime",
      "cpu-used" = 8,
      threads = 4,
      "row-mt" = 1
    )
  ),
  out,
  s
)
