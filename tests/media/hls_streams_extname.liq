%include "../test.liq"
%include "hls_lib.liq"

# operator.md §4: `streams()` reports the media segments' extension, "m4s"
# for an fMP4 variant.
dir = file.temp_dir(cleanup=true, "hls-streams-extname")

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    segment_duration=1.,
    dir,
    [
      (
        "fmp4",
        %ffmpeg(
          format = "mp4",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      ),
      (
        "ts",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

def extname(name) =
  list.find(fun (v) -> v.name == name, o.streams()).extname
end

def check() =
  hls_test.check(
    extname("fmp4") == "m4s",
    "fmp4: extname #{extname("fmp4")}, expected m4s"
  )
  hls_test.check(
    extname("ts") == "ts",
    "ts: extname #{extname("ts")}, expected ts"
  )
  hls_test.done(2)
end

started = ref(false)
o.on_file_change(
  synchronous=true,
  fun (ev) ->
    if
      not started() and path.basename(ev.path) == "fmp4_1.m4s"
    then
      started := true
      thread.run(check)
    end
)
