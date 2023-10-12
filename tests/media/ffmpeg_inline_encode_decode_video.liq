log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

out_copy = "ffmpeg_inline_encode_decode_video_copy.mp4"

if file.exists(out_copy) then
  file.remove(out_copy)
end

out_encode = "ffmpeg_inline_encode_decode_video_encode.mp4"
if file.exists(out_encode) then
  file.remove(out_encode)
end

v = video.fill(color=0xff0000, blank())
a = sine(duration=10.)
s = source.mux.video(video=v, a)

todo = ref(2)

def on_close (_) =
  def check(out) =
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(out)}")

    let json.parse ( parsed : {
      streams: [{
        codec_name: string,
        pix_fmt: string
      }]
    }) = j

    let [stream] = parsed.streams

    stream.codec_name == "h264" and
    stream.pix_fmt == "yuv420p"
  end

  todo := !todo - 1

  if not test.done () and !todo == 0 then
    if check(out_copy) and check(out_encode) then
      test.pass()
    else
      test.fail()
    end
  end
end

s = ffmpeg.encode.video(
  %ffmpeg(
    %video(codec="libx264")
  ),
  s
)

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    %video.copy,
  ),
  out_copy,
  s
)

s = ffmpeg.decode.video(s)

#clock.assign_new(sync='none',[s])

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    %video(codec="libx264")
  ),
  out_encode,
  s
)
