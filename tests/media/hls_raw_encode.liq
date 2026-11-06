%include "../test.liq"
%include "hls_lib.liq"

# Raw FFmpeg frames from ffmpeg.raw.encode.audio and
# ffmpeg.raw.encode.audio_video, encoded by `%audio.raw` and `%video.raw`
# variants in MPEG-TS and fMP4.
settings.frame.video.width := 320
settings.frame.video.height := 240

root = file.temp_dir(cleanup=true, "hls-raw")

av =
  source(
    {
      audio = source.tracks(sine()).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
  )
av = ffmpeg.raw.encode.audio_video(%ffmpeg(%audio.raw, %video.raw), av)
a = ffmpeg.raw.encode.audio(%ffmpeg(%audio.raw), sine())
clock.assign_new(sync="none", [av])
clock.assign_new(sync="none", [a])

pending = ref(2)

def check_variant(dir, name, video) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  hls_test.check(
    list.length(p.segments) >= 5,
    "#{name}: #{list.length(p.segments)} segments"
  )
  list.iteri(
    fun (i, seg) ->
      begin
        if
          i < list.length(p.segments) - 1
        then
          hls_test.check(
            abs(seg.duration - 2.) <= 0.05,
            "#{name}: #{seg.uri} lasts #{seg.duration}: #{
              list.map(fun (s) -> s.duration, p.segments)
            }"
          )
        end
        init_path = seg.map == "" ? "" : "#{dir}/#{seg.map}"
        packets = hls_test.packets(init=init_path, "#{dir}/#{seg.uri}")
        audio = list.filter(fun (pk) -> pk.type == "audio", packets)
        v = list.filter(fun (pk) -> pk.type == "video", packets)
        hls_test.check(
          audio != [] and (video ? (v != [] and list.hd(v).key) : (v == [])),
          "#{name}: #{seg.uri} has #{list.length(audio)} audio and #{
            list.length(v)
          } video packets"
        )
      end,
    p.segments
  )
end

# One output per raw source, each stopped after its sixth segment.
def make(name, video, variants, s) =
  dir = "#{root}/#{name}"
  o =
    output.file.hls(
      fallible=true,
      segment_duration=2.,
      segments=100,
      segments_overhead=null,
      dir,
      variants,
      s
    )
  last = fst(list.hd(variants))
  o.on_file_change(
    synchronous=false,
    fun (ev) ->
      if
        ev.state == "created"
        and string.contains(prefix="#{dir}/#{last}_6.", ev.path)
      then
        o.stop()
      end
  )
  hls_test.at_first_stop(
    o,
    fun () ->
      begin
        list.iter(fun (v) -> check_variant(dir, fst(v), video), variants)
        pending := pending() - 1
        if pending() == 0 then hls_test.done(40) end
      end
  )
end

make(
  "av",
  true,
  [
    (
      "av_ts",
      %ffmpeg(
        format = "mpegts",
        %audio.raw(codec = "aac", samplerate = 44100, channels = 2),
        %video.raw(codec = "libx264", preset = "ultrafast")
      )
    ),
    (
      "av_mp4",
      %ffmpeg(
        format = "mp4",
        %audio.raw(codec = "aac", samplerate = 44100, channels = 2),
        %video.raw(codec = "libx264", preset = "ultrafast")
      )
    )
  ],
  av
)
make(
  "a",
  false,
  [
    (
      "a_ts",
      %ffmpeg(
        format = "mpegts",
        %audio.raw(codec = "aac", samplerate = 44100, channels = 2)
      )
    ),
    (
      "a_adts",
      %ffmpeg(
        format = "adts",
        %audio.raw(codec = "aac", samplerate = 44100, channels = 2)
      )
    )
  ],
  a
)
