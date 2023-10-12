out = "concat.mp4"

s1 = single("first-concat.mp4")
s2 = single("second-concat.mp4")
s3 = single("third-concat.mp4")

s = sequence([s1,s2,s3,switch([])])

#clock.assign_new(sync='none',[s])

def on_close (filename) =
  if not test.done() then
    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

    let json.parse {
      streams = [
        {duration}
      ]
    } = ojson

    # Actual time is 59.93..
    if int(float_of_string(duration)) == 59 then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(%video.copy),
  out,
  s
)
