#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
audio.samplerate.set(48000)

debian_version = string.trim(process.run("cat /etc/os-release | grep VERSION_ID | cut -d'=' -f 2 | xargs").stdout)

if debian_version == "10" then
  test.skip()
end

main_encoder =
  %ffmpeg(
    %audio(
      codec="aac",
      b="128k",
      channels=2,
      ar=44100
    ),
    %video(
      codec="libx264",
      b="5M",
      flags="+global_header"
    )
  )

mpegts = %ffmpeg(
  format="mpegts",
  %audio.copy, %video.copy)

mp4 = %ffmpeg(
  format="mp4",
  movflags="+dash+skip_sidx+skip_trailer+frag_custom",
  frag_duration=2,
  %audio.copy, %video.copy)

s = noise()

encoded = ffmpeg.encode.audio_video(main_encoder, s)

streams = [
  ("mp4", mp4),
  ("mpegts",mpegts)
]

output_dir = file.temp_dir("liq","hls")

state_file = path.concat(path.dirname(argv(0)), "test_ffmpeg_distributed_hls_state.json")
process.run("cp #{state_file} #{output_dir}/state.json")

def cleanup() =
  file.rmdir(output_dir)
end

on_shutdown(cleanup)

is_done = ref(false)

def check_stream() =
  if not !is_done then
    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{output_dir}/mp4.m3u8")

    output_format = json.parse(default=[("streams", [[("samplerate", "0")]])], ojson)

    output_streams = list.assoc(default=[], "streams", output_format)

    params = ["channel_layout", "sample_rate",
              "sample_fmt", "codec_name", "pix_fmt"]

    def m(s) =
      def f(e) =
        let (p, _) = e
        list.mem(p, params)
      end
      list.filter(f, s)
    end

    output_streams = list.map(m, output_streams)

    def cmp(c, c') =
      if c < c' then
        -1
      elsif c' < c then
        1
      else
        0
      end
    end

    output_streams = list.map(list.sort(cmp), output_streams)

    def cmd_l(l, l') =
      cmp(list.assoc("codec_name", l), list.assoc("codec_name", l'))
    end

    output_streams = list.sort(cmd_l, output_streams)

    expected = [
      [("channel_layout", "stereo"), ("codec_name", "aac"), ("sample_fmt", "fltp"), ("sample_rate", "44100")],
      [("codec_name", "h264"), ("pix_fmt", "yuv420p")]
    ]

    is_done := true

    if output_streams == expected then
      test.pass()
    else
      test.fail()
    end
  end
end

def segment_name(~position,~extname,stream_name) =
  if position > 10 then
    check_stream()
  end

  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

output.file.hls(playlist="live.m3u8",
                segment_duration=2.0,
                segments=5,
                segments_overhead=5,
                segment_name=segment_name,
                output_dir,
                persist_at="#{output_dir}/state.json",
                strict_persist=true,
                streams,
                fallible=true,
                encoded)
