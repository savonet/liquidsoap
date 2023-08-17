log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

test.skip()

fname = argv(default="",1)
out = "#{fname}+ffmpeg_filter.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = sequence([s,s,s,s,s,source.fail()])

s = video.add_text.ffmpeg("bla blo", s)

started = ref(false)

def on_track(_) =
  started := true
end

s.on_track(on_track)

#clock.assign_new(id='test_clock',sync='none',[s])

def on_close(filename) =
  if !started then
    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

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

    output_streams = list.map((fun (stream) -> list.sort(cmp, stream)), output_streams)

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

output.file(fallible=true, on_close=on_close, %ffmpeg(format="mkv",%audio(codec="aac"),%video(codec="libx264")), out, s)
