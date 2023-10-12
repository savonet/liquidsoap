log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

out_copy = "ffmpeg_inline_encode_decode_audio_copy.mp4"

if file.exists(out_copy) then
  file.remove(out_copy)
end

out_encode = "ffmpeg_inline_encode_decode_audio_encode.mp4"
if file.exists(out_encode) then
  file.remove(out_encode)
end

s = once(blank(duration=10.))

todo = ref(2)

def on_close (_) =
  def check(out) =
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(out)}")

    let json.parse ( parsed : {
      streams: [{
        channel_layout: string,
        sample_rate: string,
        sample_fmt: string,
        codec_name: string,
      }]
    }) = j

    let [stream] = parsed.streams

    stream.channel_layout == "stereo" and
    stream.codec_name == "aac" and
    stream.sample_fmt == "fltp" and
    stream.sample_rate == "44100"
  end

  todo := !todo - 1

  if not test.done() and !todo == 0 then
    if check(out_copy) and check(out_encode) then
      test.pass()
    else
      test.fail()
    end
  end
end

s = ffmpeg.encode.audio(
  %ffmpeg(
    %audio(codec="aac")
  ),
  s
)

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    %audio.copy,
  ),
  out_copy,
  s
)

s = ffmpeg.decode.audio(s)

#clock.assign_new(sync='none',[s])

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    %audio(codec="aac")
  ),
  out_encode,
  s
)
