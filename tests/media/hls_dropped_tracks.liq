%include "../test.liq"
%include "hls_lib.liq"

# variants.md §2: from a source with two audio tracks and a video track, each
# variant keeps the tracks it does not drop, and only those.
settings.frame.video.width := 320
settings.frame.video.height := 240
dir = file.temp_dir(cleanup=true, "hls-dropped")

s =
  source(
    {
      audio = source.tracks(sine(440.)).audio,
      audio_2 = source.tracks(sine(880.)).audio,
      video = source.tracks(video.testsrc.ffmpeg()).video
    }
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
        "first",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %audio_2.drop,
          %video.drop
        )
      ),
      (
        "second_video",
        %ffmpeg(
          format = "mpegts",
          %audio.drop,
          %audio_2(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", preset = "ultrafast")
        )
      ),
      (
        "both_audio",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %audio_2(codec = "aac", samplerate = 44100, channels = 2),
          %video.drop
        )
      )
    ],
    s
  )

def streams(seg_path, kind) =
  list.fold(
    fun (l, pk) ->
      pk.type == kind and not list.mem(pk.stream, l) ? [...l, pk.stream] : (l),
    [],
    hls_test.packets(seg_path)
  )
end

def check_variant(name, audio, video) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  hls_test.check(
    list.length(p.segments) >= 5,
    "#{name}: #{list.length(p.segments)} segments"
  )
  last = list.length(p.segments) - 1

  # The last segment, closed at the stop, may miss a track's tail.
  list.iteri(
    fun (i, seg) ->
      begin
        a = list.length(streams("#{dir}/#{seg.uri}", "audio"))
        v = list.length(streams("#{dir}/#{seg.uri}", "video"))
        hls_test.check(
          i == last ? (a <= audio and v <= video) : (a == audio and v == video),
          "#{name}: #{seg.uri} has #{a} audio and #{v} video streams, expected #{
            audio
          } and #{video}"
        )
      end,
    p.segments
  )
end

o.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and path.basename(ev.path) == "both_audio_6.ts"
    then
      o.stop()
    end
)

hls_test.at_first_stop(
  o,
  fun () ->
    begin
      check_variant("first", 1, 0)
      check_variant("second_video", 1, 1)
      check_variant("both_audio", 2, 0)
      hls_test.done(20)
    end
)
