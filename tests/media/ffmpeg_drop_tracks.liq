output_dir = file.temp_dir("liq", "hls")

def cleanup() =
  file.rmdir(output_dir)
end

input_a = sine(duration=10.)
input_b = sine(duration=10.)
input_c = sine(duration=10.)

let {audio = audio_a} = source.tracks(input_a)
let {audio = audio_b} = source.tracks(input_b)
let {audio = audio_c} = source.tracks(input_c)

muxed_input = source({audio_a=audio_a, audio_b=audio_b, audio_c=audio_c})

streams =
  [
    (
      "a",
      %ffmpeg(
        format = "mpegts",
        %audio_a(codec = "aac"),
        %audio_b.drop,
        %audio_c.drop
      )
    ),
    (
      "b",
      %ffmpeg(
        format = "mpegts",
        %audio_a.drop,
        %audio_b(codec = "aac"),
        %audio_c.drop
      )
    ),
    (
      "c",
      %ffmpeg(
        format = "mpegts",
        %audio_a.drop,
        %audio_b.drop,
        %audio_c(codec = "aac")
      )
    )
  ]

is_done = ref(false)

def check_stream() =
  if
    not is_done()
  then
    is_done := true

    def check_stream(name) =
      stream = process.quote("#{output_dir}/#{name}.m3u8")

      file.write(append=true, data="\r\n#EXT-X-ENDLIST\r\n", stream)

      ojson =
        process.read(
          "ffprobe -v quiet -print_format json -show_streams #{stream}"
        )

      let json.parse (parsed :
        {
          streams: [
            {
              channel_layout: string?,
              sample_rate: string?,
              sample_fmt: string?,
              codec_name: string?
            }
          ]
        }
      ) = ojson

      audio_stream =
        list.find(
          (fun (stream) -> null.defined(stream.sample_rate)), parsed.streams
        )

      null.get(audio_stream.channel_layout) == "stereo"
    and
      null.get(audio_stream.codec_name) == "aac"
    and
      null.get(audio_stream.sample_fmt) == "fltp"
    and
      null.get(audio_stream.sample_rate) == "44100"
    end

    if
      check_stream("a") and check_stream("b") and check_stream("c")
    then
      test.pass()
    else
      test.fail()
    end
  end
end

def segment_name(metadata) =
  let {position, extname, stream_name} = metadata
  if position > 2 then check_stream() end
  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

output.file.hls(
  fallible=true,
  playlist="live.m3u8",
  segment_duration=2.0,
  segments=5,
  segments_overhead=5,
  segment_name=segment_name,
  output_dir,
  streams,
  muxed_input
)
