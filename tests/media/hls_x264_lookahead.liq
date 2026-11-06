%include "../test.liq"
%include "hls_lib.liq"

# KC1: libx264 with its default lookahead and B-frames, keyint 12 on 2 s
# segments and keyint 25 on 1 s segments: each segment but the last lasts the
# segment duration within one frame, without alternating and without a
# forced split.
settings.frame.video.width := 320
settings.frame.video.height := 240
log_path = file.temp("hls-x264", ".log")
log.file.path := log_path
log.file := true

root = file.temp_dir(cleanup=true, "hls-x264")
s =
  source(
    {
      audio = source.tracks(sine()).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
clock.assign_new(sync="none", [s])

frame = 1. / 25.
tolerance = frame + 0.001
eps = 0.001
stopped = ref(0)

def check(name, dir, duration) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 10,
    "#{name}: only #{n} segments listed"
  )
  durations = list.map(fun (seg) -> seg.duration, p.segments)
  list.iteri(
    fun (i, d) ->
      if
        i < n - 1
      then
        hls_test.check(
          abs(d - duration) <= tolerance,
          "#{name}: segment #{i} lasts #{d}, expected #{duration}: #{durations}"
        )
        if
          i < n - 2
        then
          d' = list.nth(durations, i + 1)
          hls_test.check(
            not (d < duration - eps and d' > duration + eps)
            and not (d > duration + eps and d' < duration - eps),
            "#{name}: segments #{i} and #{i + 1} alternate: #{durations}"
          )
        end
      end,
    durations
  )
  key_first =
    list.for_all(
      fun (seg) ->
        begin
          video =
            list.filter(
              fun (pk) -> pk.type == "video",
              hls_test.packets("#{dir}/#{seg.uri}")
            )
          video != [] and list.hd(video).key
        end,
      p.segments
    )
  hls_test.check(
    key_first,
    "#{name}: a segment does not start with a key frame"
  )
end

def finish() =
  forced =
    hls_test.log_lines(
      log_path,
      "forced split"
    )
  hls_test.check(
    forced == [],
    "forced splits logged: #{forced}"
  )
  hls_test.done(40)
end

def make(name, duration, keyint) =
  dir = "#{root}/#{name}"
  o =
    output.file.hls(
      fallible=true,
      segment_duration=duration,
      segments=100,
      segments_overhead=null,
      dir,
      [
        (
          name,
          %ffmpeg(
            format = "mpegts",
            %audio(codec = "aac", samplerate = 44100, channels = 2),
            %video(codec = "libx264", g = keyint)
          )
        )
      ],
      s
    )
  o.on_file_change(
    synchronous=false,
    fun (ev) ->
      if
        ev.state == "created" and path.basename(ev.path) == "#{name}_14.ts"
      then
        o.stop()
      end
  )
  hls_test.at_first_stop(
    o,
    fun () ->
      begin
        check(name, dir, duration)
        ref.incr(stopped)
        if stopped() == 2 then finish() end
      end
  )
end

make("g12", 2., 12)
make("g25", 1., 25)
