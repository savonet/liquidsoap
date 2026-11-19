%include "../test.liq"
%include "hls_lib.liq"

# KC1: a video with key frames every 4 s, in 1 s segments. Re-encoded with
# the same key-frame interval, then copied: segments are cut at key frames
# only, last 4 s, and the output warns. Re-encoded with no key-frame interval
# in the format: the default interval gives 1 s segments.
fixture = argv(1)
log_path = file.temp("hls-bound", ".log")
log.file.path := log_path
log.file := true

def make(name, format) =
  dir = file.temp_dir(cleanup=true, "hls-bound-#{name}")
  s = once(single(fixture))
  clock.assign_new(sync="none", [s])
  o =
    output.file.hls(
      fallible=true,
      segment_duration=1.,
      segments=100,
      segments_overhead=null,
      dir,
      [(name, format)],
      s
    )
  (o, dir)
end

let (encoded, encoded_dir) =
  make(
    "encoded",
    %ffmpeg(
      format = "mpegts",
      %audio(codec = "aac", samplerate = 44100, channels = 2),
      %video(codec = "libx264", g = 100, sc_threshold = 0)
    )
  )

let (copied, copied_dir) =
  make("copied", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))

let (defaulted, defaulted_dir) =
  make(
    "defaulted",
    %ffmpeg(
      format = "mpegts",
      %audio(codec = "aac", samplerate = 44100, channels = 2),
      %video(codec = "libx264")
    )
  )

checked = ref(0)

def finish() =
  ref.incr(checked)
  if checked() == 3 then hls_test.done(30) end
end

def video_key(dir, seg) =
  firsts = hls_test.first_packets(hls_test.packets("#{dir}/#{seg.uri}"))
  list.exists(fun (p) -> p.type == "video" and p.key, firsts)
end

def check_sparse(name, dir) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 4,
    "#{name}: only #{n} segments"
  )
  list.iteri(
    fun (i, seg) ->
      begin
        hls_test.check(
          video_key(dir, seg),
          "#{name}: #{seg.uri} does not start with a video key frame"
        )
        if
          i < n - 1
        then
          hls_test.check(
            abs(seg.duration - 4.) < 0.1,
            "#{name}: #{seg.uri} lasts #{seg.duration}, not one 4 s key-frame \
             interval"
          )
        end
      end,
    p.segments
  )
  warnings =
    list.length(
      hls_test.log_lines(
        log_path,
        "Variant #{name}: segment of"
      )
    )
  hls_test.check(
    warnings >= 1,
    "#{name}: no warning about segments over the target duration"
  )
  finish()
end

def check_defaulted() =
  p = hls_test.playlist("#{defaulted_dir}/defaulted.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 15,
    "defaulted: only #{n} segments"
  )
  list.iteri(
    fun (i, seg) ->
      begin
        hls_test.check(
          video_key(defaulted_dir, seg),
          "defaulted: #{seg.uri} does not start with a video key frame"
        )
        if
          i < n - 1
        then
          hls_test.check(
            abs(seg.duration - 1.) <= 0.04,
            "defaulted: #{seg.uri} lasts #{seg.duration}, not 1 s"
          )
        end
      end,
    p.segments
  )
  finish()
end

hls_test.at_first_stop(encoded, {check_sparse("encoded", encoded_dir)})
hls_test.at_first_stop(copied, {check_sparse("copied", copied_dir)})
hls_test.at_first_stop(defaulted, check_defaulted)
