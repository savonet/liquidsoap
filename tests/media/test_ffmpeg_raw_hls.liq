#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level", 5)
set("frame.audio.samplerate",48000)

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

s = noise(duration=1.)

raw = ffmpeg.raw.audio_video(raw_encoder, s)

streams = [
  ("mp4", mp4),
  ("mpegts",mpegts)
]

is_ready = ref(false)

def segment_name(~position,~extname,stream_name) =
  is_ready := position > 2
  timestamp = int_of_float(time())
  "#{stream_name}_#{timestamp}_#{position}.#{extname}"
end

output_dir = file.temp_dir("liq","hls")

def cleanup() =
  file.rmdir(output_dir)
end

on_shutdown(cleanup)

def check_stream(_) =
  if !is_ready then
    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{output_dir}/mp4.m3u8")

    output_format = of_json(default=[("streams", [[("samplerate", "0")]])], ojson)

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

    if output_streams == expected then
      test.pass()
    else
      test.fail()
    end
  end
end

s.on_track(check_stream)

output.file.hls(playlist="live.m3u8",
                segment_duration=2.0,
                segments=5,
                segments_overhead=5,
                segment_name=segment_name,
                output_dir,
                streams,
                fallible=true,
                raw)
