log.level := 4

output_dir = file.temp_dir("liq", "hls")

def cleanup() =
  file.rmdir(output_dir)
end

fname = argv(default="", 1)

s = single(fname)

stream = %ffmpeg(format = "mp4", %video.copy, %audio.copy)

streams = [("mp4", stream)]

is_done = ref(false)

def check_stream() =
  if
    not !is_done
  then
    is_done := true

    ojson =
      process.read(
        "ffprobe -v quiet -print_format json -show_streams #{
          output_dir
        }/mp4.m3u8"
      )

    let json.parse (parsed : {streams: [{codec_type: string}]}) = ojson

    has_video_stream =
      list.exists(
        (fun (stream) -> stream.codec_type == "video"),
        parsed.streams
      )

    has_audio_stream =
      list.exists(
        (fun (stream) -> stream.codec_type == "audio"),
        parsed.streams
      )

    if
      has_video_stream and has_audio_stream
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
