%include "../test.liq"

# A stream input decodes with the preferred decoder: libvpx-vp9 keeps the
# alpha plane that FFmpeg's native VP9 decoder drops.

log.level.set(4)

settings.decoder.ffmpeg.codecs.vp9.set("libvpx-vp9")

fname = argv(default="", 1)
output_path = file.temp(cleanup=true, "vp9-alpha-stream", ".webm")
port = 18764

harbor = input.harbor(port=port, buffer=0., "vp9-alpha")
connected = ref(false)
ignore(harbor.on_connect(synchronous=true, fun (_) -> connected := true))
s = max_duration(1., harbor)

# Harbor closes the upload once it has the second it needs, so curl's exit
# code does not tell a good upload from a rejected one. Whether harbor took the
# source does.
def upload() =
  upload =
    process.run(
      "curl -sS -T #{process.quote(fname)} -H 'Expect:' -H 'Content-Type: \
       video/webm' -u source:hackme http://localhost:#{port}/vp9-alpha"
    )
  if
    not connected()
  then
    print(
      "Upload rejected or failed: #{upload.status} #{upload.status.code} #{
        upload.stderr
      }"
    )
    test.fail()
  end
end

thread.run(delay=1., upload)

thread.run(delay=60., fun () -> test.fail())

done = ref(false)

def on_close(filename) =
  if
    not done()
  then
    done := true

    probe_output =
      process.read(
        "ffprobe -c:v libvpx-vp9 -v error -select_streams v:0 -show_entries \
         stream=pix_fmt:format=duration -of json #{process.quote(filename)}"
      )

    let json.parse (parsed :
      {streams: [{pix_fmt: string?}], format: {duration: string}}
    ) = probe_output

    pix_fmt = null.get(default="", (list.hd(parsed.streams)).pix_fmt)
    probed_duration = float_of_string(default=0., parsed.format.duration)

    if
      pix_fmt == "yuva420p" and probed_duration > 0.
    then
      test.pass()
    else
      print(
        "Expected yuva420p with a positive duration, got #{pix_fmt} and #{
          probed_duration
        }s"
      )
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
  output_path,
  s
)
