%include "../test.liq"
%include "hls_lib.liq"

# files.md §2: an fMP4 variant whose `extname` is "m4s" names its init
# segments ".mp4", and survives a stop and a start.
dir = file.temp_dir(cleanup=true, "hls-fmp4-extname")

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "a",
        %ffmpeg(
          format = "mp4",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        ).{extname = "m4s"}
      )
    ],
    s
  )

created = ref([])
restarted = ref(false)
done = ref(false)

def restart() =
  o.stop()
  o.start()
end

def check() =
  inits = list.filter(fun (n) -> r/^a_\d+\.mp4$/.test(n), created())
  segments = list.filter(fun (n) -> r/^a_\d+\.m4s$/.test(n), created())
  hls_test.check(
    list.length(inits) == 2,
    "init segments created: #{inits}, expected two .mp4 files: #{created()}"
  )
  p = hls_test.playlist("#{dir}/a.m3u8")
  maps =
    list.fold(
      fun (l, seg) -> list.mem(seg.map, l) ? l : ([...l, seg.map]),
      [],
      p.segments
    )
  hls_test.check(
    list.length(maps) == 2 and list.for_all(fun (m) -> r/\.mp4$/.test(m), maps),
    "EXT-X-MAP URIs: #{maps}"
  )
  hls_test.check(
    list.length(list.filter(fun (seg) -> seg.discontinuous, p.segments)) == 1,
    "expected one discontinuity after the restart"
  )
  hls_test.check(
    list.length(segments) >= 8,
    "media segments created: #{segments}"
  )
  list.iter(
    fun (seg) ->
      hls_test.check(
        list.exists(
          fun (pk) -> pk.type == "audio",
          hls_test.packets(init="#{dir}/#{seg.map}", "#{dir}/#{seg.uri}")
        ),
        "#{seg.uri} with #{seg.map}: no audio packet"
      ),
    p.segments
  )
  hls_test.done(12)
end

o.on_file_change(
  synchronous=true,
  fun (ev) ->
    begin
      name = path.basename(ev.path)
      if ev.state == "created" then created := [...created(), name] end
      if
        not restarted() and name == "a_4.m4s"
      then
        restarted := true
        thread.run(restart)
      end
      if
        not done() and ev.state == "created" and name == "a_10.m4s"
      then
        done := true
        thread.run(o.stop)
      end
    end
)

stops = ref(0)
o.on_stop(
  synchronous=false,
  fun () ->
    begin
      ref.incr(stops)
      if stops() == 2 then thread.run(check) end
    end
)
