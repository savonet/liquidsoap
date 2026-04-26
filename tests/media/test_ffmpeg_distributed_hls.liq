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

fname = argv(default="", 1)

main_encoder =
  %ffmpeg(
    %audio(codec = "aac", b = "128k", channels = 2, ar = 44100),
    %video(codec = "libx264", b = "5M", flags = "+global_header")
  )

mpegts = %ffmpeg(format = "mpegts", %audio.copy, %video.copy)
mp4 =
  %ffmpeg(
    format = "mp4",
    movflags = "+dash+skip_sidx+skip_trailer+frag_custom",
    frag_duration = 2,
    %audio.copy,
    %video.copy
  )

s = single(fname)
encoded = ffmpeg.encode.audio_video(main_encoder, s)
streams = [("mp4", mp4), ("mpegts", mpegts)]

# Phase 1 output dir (fixed name, dune directory target).
setup_dir = "hls_resume_output"
output_dir = file.temp_dir("liq", "hls")

# Copy Phase 1 segments and state into our working dir so that the resumed
# playlist can reference the old segment files by their original basenames.
ignore(
  process.run(
    "cp -r #{setup_dir}/. #{output_dir}/"
  )
)

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
    ) = test.ffprobe("#{output_dir}/mp4.m3u8")

    video_stream =
      list.find((fun (stream) -> stream.codec_name == "h264"), parsed.streams)

    audio_stream =
      list.find((fun (stream) -> stream.codec_name == "aac"), parsed.streams)

    if
      null.get(video_stream.codec_name) == "h264"
      and null.get(audio_stream.channel_layout) == "stereo"
      and null.get(audio_stream.codec_name) == "aac"
      and null.get(audio_stream.sample_fmt) == "fltp"
      and null.get(audio_stream.sample_rate) == "44100"
    then
      test.pass()
    else
      test.fail()
    end
  end
end

first_position = ref(-1)

def segment_name(metadata) =
  let {position, extname, stream_name} = metadata
  if first_position() < 0 then first_position := position end
  if position > first_position() + 5 then check_stream() end
  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

output.file.hls(
  playlist="live.m3u8",
  segment_duration=2.0,
  segments=5,
  segments_overhead=5,
  segment_name=segment_name,
  output_dir,
  persist_at="#{output_dir}/state.json",
  strict_persist=true,
  streams,
  fallible=true,
  encoded
)
