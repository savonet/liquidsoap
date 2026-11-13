%include "../test.liq"
%include "hls_lib.liq"

# persistence.md: an inline-encoded source copied into an MPEG-TS variant,
# run twice in child processes with the same `persist_at`: the second run
# continues the numbering, its first segment is discontinuous, and every
# listed segment decodes.
dir = file.temp_dir(cleanup=true, "hls-copy-resume")
out = "#{dir}/out"
stdlib = "../../src/libs/stdlib.liq"
liquidsoap_command =
  process.quote(liquidsoap.executable) ^
    (
      file.exists(stdlib)
        ? " --stdlib #{stdlib}"
        : ""
    )

script = "#{dir}/run.liq"
file.write(
  data='settings.frame.video.width := 320
settings.frame.video.height := 240
s = source({audio = source.tracks(sine()).audio, video = source.tracks(video.testsrc.ffmpeg()).video})
s = ffmpeg.encode.audio_video(%ffmpeg(%audio(codec = "aac", samplerate = 44100, channels = 2), %video(codec = "libx264", preset = "ultrafast", g = 50, flags = "+global_header")), s)
clock.assign_new(sync="none", [s])
o = output.file.hls(fallible=true, segment_duration=2., segments=100, segments_overhead=null, persist_at="#{
    out
  }/state.json", "#{
    out
  }", [("v", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))], s)
created = ref(0)
o.on_file_change(synchronous=false, fun (ev) -> if ev.state == "created" and r/\\.ts$/.test(ev.path) then begin ref.incr(created) if created() == 5 then shutdown() end end end)
',
  script
)

def run(n) =
  r =
    process.run(
      timeout=120.,
      "#{liquidsoap_command} #{process.quote(script)} 2>&1"
    )
  hls_test.check(
    r.status == "exit" and r.status.code == 0,
    "run #{n}: #{r.status} #{r.status.code}: #{r.stdout}"
  )
  hls_test.playlist("#{out}/v.m3u8")
end

def position(uri) =
  int_of_string(list.assoc(default="-1", 1, r/^v_(\d+)\.ts$/.exec(uri)))
end

def check() =
  first = run(1)
  first_count = list.length(first.segments)
  hls_test.check(
    first_count >= 5,
    "run 1 listed #{first_count} segments"
  )
  last_first = position(list.nth(first.segments, first_count - 1).uri)
  second = run(2)
  uris = list.map(fun (seg) -> seg.uri, second.segments)
  hls_test.check(
    list.prefix(first_count, uris) ==
      list.map(fun (seg) -> seg.uri, first.segments),
    "run 2 does not keep run 1's segments: #{uris}"
  )
  hls_test.check(
    list.length(second.segments) > first_count,
    "run 2 added no segment: #{uris}"
  )
  resumed = list.nth(second.segments, first_count)
  hls_test.check(
    position(resumed.uri) == last_first + 1,
    "run 2 starts at #{resumed.uri}, run 1 ended at position #{last_first}"
  )
  hls_test.check(
    resumed.discontinuous,
    "#{resumed.uri}, the first of run 2, is not discontinuous"
  )
  discontinuities = list.filter(fun (seg) -> seg.discontinuous, second.segments)
  hls_test.check(
    list.length(discontinuities) == 1,
    "#{list.length(discontinuities)} discontinuities: #{uris}"
  )
  list.iter(
    fun (seg) ->
      begin
        packets = hls_test.packets("#{out}/#{seg.uri}")
        video = list.filter(fun (pk) -> pk.type == "video", packets)
        audio = list.filter(fun (pk) -> pk.type == "audio", packets)
        hls_test.check(
          video != [] and audio != [] and list.hd(video).key,
          "#{seg.uri}: #{list.length(video)} video, #{list.length(audio)} audio \
           packets"
        )
      end,
    second.segments
  )
  hls_test.done(18)
end

thread.run(check)
