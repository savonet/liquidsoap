log.level := 4

output_dir = file.temp_dir("liq", "hls")

def cleanup() =
  file.rmdir(output_dir)
end

fname = argv(default="", 1)

s = single(fname)

stream =
  %ffmpeg(
    format = "mp4",
    %video(codec = "libx264", x264opts = "keyint=10:min-keyint=10:no-scenecut"),
    %audio(codec = "aac")
  )

streams = [("mp4", stream)]

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
  if position > 10 then check_stream() end
  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

clock.assign_new(id='test_clock', sync='none', [s])
output.file.hls(
  fallible=true,
  segment_duration=2.,
  segment_name=segment_name,
  output_dir,
  streams,
  s
)
