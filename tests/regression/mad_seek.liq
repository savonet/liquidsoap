%include "../test.liq"

# The mad decoder's frame skipping did not stop after one frame, so every
# forward seek went to the end of the file (#3584).

settings.decoder.decoders := ["mad"]

dir = file.temp_dir("mad_seek")
on_cleanup({file.rmdir(dir)})
mp3 = "#{dir}/tone.mp3"
ignore(
  process.run(
    "ffmpeg -v error -y -f lavfi -i sine=d=30 -c:a libmp3lame -b:a 128k #{
      process.quote(mp3)
    }"
  )
)

s = once(single(mp3))
clock.assign_new(sync="none", [s])

def check() =
  seeked = s.seek(15.)
  print(
    "seek(15.) moved by #{seeked}"
  )
  if 14. < seeked and seeked < 16. then test.pass() else test.fail() end
end

checked = ref(false)
s.on_frame(
  synchronous=true,
  fun () ->
    if
      not checked() and s.time() > 0.5
    then
      checked := true
      check()
    end
)

output.dummy(fallible=true, s)
