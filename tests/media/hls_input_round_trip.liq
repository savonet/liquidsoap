%include "../test.liq"
%include "hls_lib.liq"

# An AAC MPEG-TS variant with timed ID3, read back from disk with input.hls
# once the output stops: media plays and the title set on the output side
# comes back as metadata.
dir = file.temp_dir(cleanup=true, "hls-round-trip")

s = sequence(list.init(30, fun (_) -> once(sine(duration=1.))))
s =
  metadata.map(
    insert_missing=true,
    fun (_) ->
      [
        (
          "title",
          "round trip"
        ),
        ("artist", "liquidsoap")
      ],
    s
  )
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=2.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "aac",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

titles = ref([])

def wait_for(what, cond) =
  deadline = time() + 20.
  def rec loop() =
    if
      not cond()
    then
      if
        time() > deadline
      then
        test.fail(
          "timeout waiting for #{what}"
        )
      end
      thread.pause(0.05)
      loop()
    end
  end
  loop()
end

def read_back() =
  r = input.hls("#{dir}/aac.m3u8")
  r.on_metadata(
    synchronous=true,
    fun (m) -> if m["title"] != "" then titles := [...titles(), m["title"]] end
  )
  output.dummy(fallible=true, r)
  wait_for(
    "metadata read back",
    {titles() != []}
  )
  hls_test.check(
    list.for_all(
      fun (t) ->
        t ==
          "round trip",
      titles()
    ),
    "titles read back: #{titles()}"
  )
  wait_for(
    "one second of media read back",
    {r.time() >= 1.}
  )
  hls_test.check(
    r.time() >= 1.,
    "media read back: #{r.time()}s"
  )
  hls_test.done(2)
end

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "aac_8.ts"
    then
      o.stop()
    end
)

hls_test.at_first_stop(o, read_back)
