log.level.set(5)
audio.samplerate.set(48000)

debian_version = string.trim(process.run("cat /etc/os-release | grep VERSION_ID | cut -d'=' -f 2 | xargs").stdout)

if debian_version == "10" then
  test.skip()
end

raw_encoder = %ffmpeg(%audio.raw,%video.raw)

mpegts = %ffmpeg(
  format="mpegts",
  %audio.raw(
    codec="aac",
    b="128k",
    channels=2,
    ar=44100
  ),
  %video.raw(
    codec="libx264",
    b="5M",
    flags="+global_header"
  )
)

mp4 = %ffmpeg(
  format="mp4",
  movflags="+dash+skip_sidx+skip_trailer+frag_custom",
  frag_duration=2,
  %audio.raw(
    codec="aac",
    b="128k",
    channels=2,
    ar=44100
  ),
  %video.raw(
    codec="libx264",
    b="5M",
    flags="+global_header"
  )
)

s = noise()

raw = ffmpeg.raw.encode.audio_video(raw_encoder, s)

streams = [
  ("mp4", mp4),
  ("mpegts",mpegts)
]

output_dir = file.temp_dir("liq","hls")

def cleanup() =
  file.rmdir(output_dir)
end

on_cleanup(cleanup)

is_done = ref(false)

def check_stream() =
  if not !is_done then
    is_done := true

    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{output_dir}/mp4.m3u8")

    let json.parse ( parsed : {
      streams: [{
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_name: string?,
        pix_fmt: string?
      }]
    }) = ojson

    video_stream = list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)
    audio_stream = list.find((fun (stream) -> null.defined(stream.sample_rate)), parsed.streams)

    if null.get(video_stream.codec_name) == "h264" and
       null.get(video_stream.pix_fmt) == "yuv420p" and
       null.get(audio_stream.channel_layout) == "stereo" and
       null.get(audio_stream.codec_name) == "aac" and
       null.get(audio_stream.sample_fmt) == "fltp" and
       null.get(audio_stream.sample_rate) == "44100" then
      test.pass()
    else
      test.fail()
    end
  end
end

def segment_name(~position,~extname,stream_name) =
  if position > 2 then
    check_stream()
  end

  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

#clock.assign_new(sync='none',[raw])

output.file.hls(playlist="live.m3u8",
                segment_duration=2.0,
                segments=5,
                segments_overhead=5,
                segment_name=segment_name,
                output_dir,
                streams,
                fallible=true,
                raw)
