log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

out_copy = "ffmpeg_inline_encode_decode_video_copy.mp4"
if file.exists(out_copy) then file.remove(out_copy) end

out_encode = "ffmpeg_inline_encode_decode_video_encode.mp4"
if file.exists(out_encode) then file.remove(out_encode) end

let {audio} = source.tracks(sine(duration=10.))
let {video} = source.tracks(video.testsrc.ffmpeg(duration=10.))
s = source({audio=audio, video=video})

todo = ref(2)
done = ref(false)

def on_close(_) =
  def check(out) =
    let json.parse (parsed :
      {streams: [{codec_name: string, pix_fmt: string}]}
    ) = test.ffprobe(out)

    let [stream] = parsed.streams
    stream.codec_name == "h264" and stream.pix_fmt == "yuv420p"
  end

  todo := !todo - 1
  if
    not done() and !todo == 0
  then
    done := true

    if
      check(out_copy) and check(out_encode)
    then
      test.pass()
    else
      test.fail()
    end
  end
end

s = ffmpeg.encode.video(%ffmpeg(%video(codec = "libx264")), s)
output.file(fallible=true, on_close=on_close, %ffmpeg(%video.copy), out_copy, s)

s = ffmpeg.decode.video(s)

clock.assign_new(sync='none', [s])
output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(%video(codec = "libx264")),
  out_encode,
  s
)
