%include "../test.liq"
%include "hls_lib.liq"

# Every MPEG-TS segment starts with a PAT then a PMT (RFC 8216 §3.2), in an
# audio-only AAC variant and in a copied audio and video one.
fixture = argv(1)
audio_dir = file.temp_dir(cleanup=true, "hls-pat-audio")
video_dir = file.temp_dir(cleanup=true, "hls-pat-video")

audio_source = sine()
clock.assign_new(sync="none", [audio_source])
audio =
  output.file.hls(
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    audio_dir,
    [
      (
        "aac",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    audio_source
  )

video_source = once(single(fixture))
clock.assign_new(sync="none", [video_source])
video =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=100,
    segments_overhead=null,
    video_dir,
    [("av", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))],
    video_source
  )

checked = ref(0)

def check(dir, name) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 10,
    "#{name}: only #{n} segments"
  )
  list.iter(
    fun (seg) ->
      hls_test.check(
        hls_test.starts_with_pat_pmt("#{dir}/#{seg.uri}"),
        "#{name}: #{seg.uri} does not start with a PAT and a PMT"
      ),
    p.segments
  )
  ref.incr(checked)
  if checked() == 2 then hls_test.done(22) end
end

audio.on_file_change(
  synchronous=false,
  fun (ev) ->
    if
      ev.state == "created" and ev.path == "#{audio_dir}/aac_12.ts"
    then
      audio.stop()
    end
)

hls_test.at_first_stop(audio, {check(audio_dir, "aac")})
hls_test.at_first_stop(video, {check(video_dir, "av")})
