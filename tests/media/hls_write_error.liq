%include "../test.liq"
%include "hls_lib.liq"

# KC24: the output directory made unwritable mid-stream, then writable again;
# then unwritable once more and the output stopped during the outage.
fixture = argv(1)

if
  process.read(
    "id -u"
  ) ==
    "0\n"
then
  print(
    "Running as root: chmod does not block writes."
  )
  test.skip()
end

dir = file.temp_dir(cleanup=true, "hls-write-error")

s = single(fixture)
clock.assign_new(sync="none", [s])

errors = ref(0)

o =
  output.file.hls(
    reopen_on_error=fun (_) ->
      begin
        ref.incr(errors)
        0.5
      end,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "audio",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video.drop
        )
      ),
      (
        "av",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", tune = "zerolatency")
        )
      )
    ],
    s
  )

other_source = sine()
clock.assign_new(sync="none", [other_source])
other = output.dummy(other_source)
other_frames = ref(0)
other.on_frame(synchronous=true, fun () -> ref.incr(other_frames))

created = ref(0)
o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and r/\.ts$/.test(ev.path)
    then
      atomic(fun () -> ref.incr(created))
    end
)

def wait_for(what, f) =
  deadline = time() + 20.
  while not f() and time() < deadline do thread.pause(0.05)  end
  if
    not f()
  then
    test.fail(
      "timeout waiting for #{what}"
    )
  end
end

def chmod(mode) =
  ignore(
    process.run(
      "chmod #{mode} #{process.quote(dir)}"
    )
  )
end

def other_advances(what) =
  n = other_frames()
  wait_for(
    "the other output to stream #{what}",
    {other_frames() > n + 10}
  )
  hls_test.check(
    other.is_started(),
    "the other output stopped #{what}"
  )
end

def temp_files() =
  list.filter(fun (f) -> r/^\.hls-tmp-/.test(f), file.ls(dir))
end

def check_recovery(name, before, streams) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  discontinuous =
    list.filter(
      fun (i) -> list.nth(p.segments, i).discontinuous,
      list.init(list.length(p.segments), fun (i) -> i)
    )
  hls_test.check(
    list.length(discontinuous) == 1 and list.hd(discontinuous) >= before,
    "#{name}: discontinuous segments #{discontinuous}, expected one after the #{
      before
    } segments before the error"
  )
  seg = list.nth(p.segments, list.hd(discontinuous))
  firsts = hls_test.first_packets(hls_test.packets("#{dir}/#{seg.uri}"))
  hls_test.check(
    list.length(firsts) == streams and list.for_all(fun (p) -> p.key, firsts),
    "#{name}: #{seg.uri} starts with #{firsts}, not a key packet of each of its #{
      streams
    } streams"
  )
end

def run() =
  wait_for(
    "three segments",
    {created() >= 6}
  )
  segments_before =
    list.map(
      fun (name) ->
        begin
          let (name, streams) = name
          (
            name,
            list.length(hls_test.playlist("#{dir}/#{name}.m3u8").segments),
            streams
          )
        end,
      [("audio", 1), ("av", 2)]
    )

  chmod("0500")
  wait_for(
    "a write error",
    {errors() >= 1}
  )
  other_advances(
    "during the outage"
  )
  chmod("0755")

  wait_for(
    "segments after the recovery",
    fun () ->
      list.for_all(
        fun (v) ->
          begin
            let (name, before, _) = v
            list.length(hls_test.playlist("#{dir}/#{name}.m3u8").segments) >
              before + 2
          end,
        segments_before
      )
  )
  list.iter(
    fun (v) ->
      begin
        let (name, before, streams) = v
        check_recovery(name, before, streams)
      end,
    segments_before
  )
  hls_test.check(
    temp_files() == [],
    "temporary files left: #{temp_files()}"
  )
  other_advances(
    "after the recovery"
  )

  chmod("0500")
  wait_for(
    "a second write error",
    {errors() >= 2}
  )
  o.stop()
  wait_for(
    "the stop during the outage",
    {not o.is_started()}
  )
  other_advances(
    "after the stop"
  )
  chmod("0755")
  hls_test.check(
    temp_files() == [],
    "temporary files left after the stop: #{temp_files()}"
  )
  hls_test.done(9)
end

thread.run(run)
