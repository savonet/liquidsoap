log.level.set(5)
audio.samplerate.set(48000)
settings.audio.converter.samplerate.converters.set(["native"])
debian_version =
  string.trim(
    process.run(
      "cat /etc/os-release | grep VERSION_ID | cut -d'=' -f 2 | xargs"
    ).stdout
  )

if debian_version == "10" then test.skip() end

raw_encoder = %ffmpeg(%audio.raw, %video.raw)

mp4 =
  %ffmpeg(
    format = "mp4",
    %audio.raw(codec = "aac", b = "128k", channels = 2, ar = 44100),
    %video.raw(
      codec = "libx264",
      b = "5M",
      tune = "zerolatency",
      x264opts = "keyint=12:min-keyint=12"
    )
  )

a = source.tracks(sine(duration=10.)).audio
v = source.tracks(video.testsrc.ffmpeg(duration=10.)).video

s = source({audio=a, video=v})

raw = ffmpeg.raw.encode.audio_video(raw_encoder, s)

streams = [("mp4", mp4)]
output_dir = file.temp_dir("liq", "hls")

def cleanup() =
  file.rmdir(output_dir)
end

on_cleanup(cleanup)
is_done = ref(false)

def check_stream() =
  if
    not !is_done
  then
    is_done := true

    process.run("sync")

    ojson =
      process.read(
        "ffprobe -v quiet -print_format json -show_streams #{
          output_dir
        }/mp4.m3u8"
      )

    let json.parse (parsed :
      {
        streams: [
          {
            channel_layout: string?,
            sample_rate: string?,
            sample_fmt: string?,
            codec_name: string?,
            pix_fmt: string?
          }
        ]
      }
    ) = ojson

    video_stream =
      list.find((fun (stream) -> stream.codec_name == "h264"), parsed.streams)

    audio_stream =
      list.find(
        (fun (stream) -> stream.codec_name == "aac"), parsed.streams
      )

    if
      null.get(video_stream.codec_name) == "h264"
    and
      null.get(audio_stream.channel_layout) == "stereo"
    and
      null.get(audio_stream.codec_name) == "aac"
    and
      null.get(audio_stream.sample_fmt) == "fltp"
    and
      null.get(audio_stream.sample_rate) == "44100"
    then
      test.pass()
    else
      test.fail()
    end
  end
end

def segment_name(metadata) =
  let {position, extname, stream_name} = metadata
  if position > 1 then check_stream() end
  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

clock.assign_new(sync='none', [raw])
output.file.hls(
  playlist="live.m3u8",
  segment_duration=2.0,
  segments=5,
  segments_overhead=5,
  segment_name=segment_name,
  output_dir,
  streams,
  fallible=true,
  raw
)
