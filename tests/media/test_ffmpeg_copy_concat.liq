#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

out = "files/ffmpeg_copy_concat/concat.mp4"

s1 = single("files/ffmpeg_copy_concat/first-concat.mp4")
s2 = single("files/ffmpeg_copy_concat/second-concat.mp4")
s3 = single("files/ffmpeg_copy_concat/third-concat.mp4")

s = sequence([s1,s2,s3,switch([])])

clock.assign_new(sync='none',[s])

def on_done () =
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

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

output.file(
  fallible=true,
  on_stop=on_done,
  %ffmpeg(%video.copy),
  out,
  s
)
