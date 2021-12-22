#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

out = "files/ffmpeg_copy_concat/concat.mp4" 

s1 = single("files/ffmpeg_copy_concat/first-concat.mp4")
s2 = single("files/ffmpeg_copy_concat/second-concat.mp4")

s = sequence([s1,s2,switch([])])

clock.assign_new(sync='none',[s])

def on_done () =
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")
  format = json.parse(default=[("streams", [[("foo", "0")]])], ojson)
  stream = list.hd(list.assoc(default=[], "streams", format))

  # Actual time is 49.93..
  if int(float_of_string(stream["duration"])) == 49 then
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
