%include "../test.liq"
%include "hls_lib.liq"

# KC21: an encoded audio and video fMP4 variant, stopped and started once so
# that a second init segment replaces the first. The encoder has no lookahead:
# segments of the default libx264 settings exceed the target duration (KC1).
fixture = argv(1)
dir = file.temp_dir(cleanup=true, "hls-init")

s = single(fixture)
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=3,
    segments_overhead=0,
    dir,
    [
      (
        "av",
        %ffmpeg(
          format = "mp4",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", tune = "zerolatency")
        )
      )
    ],
    s
  )

events = ref([])
first_run = ref(true)
first_init = ref("")
first_segment = ref("")

# The first media segment leaves the window within a few unpaced cycles: its
# bytes and its init's are kept when it is created, and probed later.
first_init_copy = file.temp("hls-init", ".mp4")
first_segment_copy = file.temp("hls-init", ".m4s")

def check_first_segment() =
  size = string.length(file.contents(first_init_copy))
  hls_test.check(
    size > 0,
    "the init segment #{first_init()} is empty"
  )
  packets = hls_test.packets(init=first_init_copy, first_segment_copy)
  video = list.filter(fun (p) -> p.type == "video", packets)
  audio = list.filter(fun (p) -> p.type == "audio", packets)
  hls_test.check(
    video != [] and audio != [],
    "#{first_segment()} holds #{list.length(video)} video and #{
      list.length(audio)
    } audio packets"
  )
  first_pts = list.fold(fun (m, p) -> p.pts < m ? p.pts : (m), infinity, video)
  hls_test.check(
    video != [] and list.hd(video).key and first_pts <= 1. / 25.,
    "#{first_segment()} does not start with the first video frame: first pts #{
      first_pts
    }"
  )
end

def restart() =
  check_first_segment()
  first_run := false
  o.stop()
  o.start()
end

o.on_file_change(
  synchronous=true,
  fun (ev) ->
    begin
      name = path.basename(ev.path)
      events := [...events(), (ev.state, name)]
      if
        first_init() == "" and r/\.mp4$/.test(name)
      then
        first_init := name
        file.write(data=file.contents(ev.path), first_init_copy)
      end
      if
        first_segment() == "" and r/\.m4s$/.test(name)
      then
        first_segment := name
        file.write(data=file.contents(ev.path), first_segment_copy)
      end
      if
        first_run() and ev.state == "created" and name == "av_4.m4s"
      then
        thread.run(restart)
      end
      if
        ev.state == "created" and name == "av_16.m4s"
      then
        thread.run(o.stop)
      end
    end
)

def index_of(state, name) =
  let (_, found) =
    list.fold(
      fun (acc, ev) ->
        begin
          let (i, found) = acc
          (i + 1, found < 0 and ev == (state, name) ? i : (found))
        end,
      (0, -1),
      events()
    )
  found
end

def check() =
  hls_test.check(
    first_init() != "",
    "no init segment was created"
  )
  init_deleted = index_of("deleted", first_init())
  hls_test.check(
    init_deleted >= 0,
    "#{first_init()} was never deleted: #{events()}"
  )
  hls_test.check(
    not file.exists("#{dir}/#{first_init()}"),
    "#{first_init()} is still on disk"
  )
  p = hls_test.playlist("#{dir}/av.m3u8")
  hls_test.check(
    not list.exists(fun (seg) -> seg.map == first_init(), p.segments),
    "#{first_init()} deleted while still listed"
  )

  # The segments created before the second init are the first init's.
  second_init = index_of("created", "av_1.mp4")
  hls_test.check(
    second_init >= 0,
    "no second init segment: #{events()}"
  )
  first_run_segments =
    list.filter_map(
      fun (ev) ->
        begin
          let (state, name) = ev
          if
            state == "created"
            and r/\.m4s$/.test(name)
            and index_of("created", name) < second_init
          then
            name
          else
            null
          end
        end,
      events()
    )
  hls_test.check(
    list.length(first_run_segments) >= 4,
    "first run segments: #{first_run_segments}"
  )
  list.iter(
    fun (name) ->
      begin
        d = index_of("deleted", name)
        hls_test.check(
          d >= 0 and d < init_deleted,
          "#{name} deleted at event #{d}, its init #{first_init()} at #{
            init_deleted
          }"
        )
      end,
    first_run_segments
  )
  on_disk =
    list.filter(fun (name) -> list.mem(name, first_run_segments), file.ls(dir))
  hls_test.check(
    on_disk == [],
    "first run segments left on disk: #{on_disk}"
  )
  hls_test.done(14)
end

stops = ref(0)
o.on_stop(
  synchronous=false,
  fun () ->
    begin
      ref.incr(stops)
      if stops() == 2 then thread.run(check) end
    end
)
