%include "../test.liq"
%include "hls_lib.liq"

# playlists.md §2: the main playlist written when a multi-variant output
# stops keeps every variant's CODECS, at a stop that is not final and at the
# final stop of a process with `persist_at`, which runs as a child process.
dir = file.temp_dir(cleanup=true, "hls-codecs-stop")
final_dir = "#{dir}/final"
stdlib = "../../src/libs/stdlib.liq"
liquidsoap_command =
  process.quote(liquidsoap.executable) ^
    (
      file.exists(stdlib)
        ? " --stdlib #{stdlib}"
        : ""
    )

variants =
  '[
  ("lo", %ffmpeg(format = "mpegts", %audio(codec = "aac", b = "64k", samplerate = 44100, channels = 2))),
  ("hi", %ffmpeg(format = "mpegts", %audio(codec = "aac", b = "128k", samplerate = 44100, channels = 2)))
]'

def check_codecs(when, main) =
  lines = r/\r\n/.split(main)
  infs =
    list.filter(
      fun (l) -> string.contains(prefix="#EXT-X-STREAM-INF:", l),
      lines
    )
  hls_test.check(
    list.length(infs) == 2,
    "#{when}: #{list.length(infs)} variants listed: #{main}"
  )
  list.iter(
    fun (l) ->
      hls_test.check(
        r/CODECS="mp4a\.40\.2"/.test(l),
        "#{when}: no AAC CODECS in #{l}"
      ),
    infs
  )
end

def run_final_stop() =
  script = "#{dir}/final.liq"
  file.write(
    data='s = sine()
clock.assign_new(sync="none", [s])
o = output.file.hls(segment_duration=1., persist_at="#{
      final_dir
    }/state.json", "#{final_dir}", #{
      variants
    }, s)
o.on_file_change(synchronous=false, fun (ev) -> if path.basename(ev.path) == "hi_5.ts" then shutdown() end)
',
    script
  )
  r =
    process.run(
      timeout=120.,
      "#{liquidsoap_command} #{process.quote(script)} 2>&1"
    )
  hls_test.check(
    r.status == "exit" and r.status.code == 0,
    "final stop run: #{r.status} #{r.status.code}: #{r.stdout}"
  )
  main = "#{final_dir}/stream.m3u8"
  hls_test.check(
    file.exists(main),
    "no main playlist after the final stop: #{file.ls(final_dir)}"
  )
  if
    file.exists(main)
  then
    check_codecs(
      "final stop",
      file.contents(main)
    )
  end
  hls_test.done(8)
end

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    "#{dir}/live",
    [
      (
        "lo",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", b = "64k", samplerate = 44100, channels = 2)
        )
      ),
      (
        "hi",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", b = "128k", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "hi_5.ts"
    then
      o.stop()
    end
)

hls_test.at_first_stop(
  o,
  fun () ->
    begin
      check_codecs("stop", file.contents("#{dir}/live/stream.m3u8"))
      run_final_stop()
    end
)
