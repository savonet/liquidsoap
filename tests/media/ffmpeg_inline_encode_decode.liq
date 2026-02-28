log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

out_copy = "ffmpeg_inline_encode_decode_copy.mp4"
if file.exists(out_copy) then file.remove(out_copy) end
out_encode = "ffmpeg_inline_encode_decode_encode.mp4"
if file.exists(out_encode) then file.remove(out_encode) end

let {audio} = source.tracks(sine(duration=10.))
let {video} = source.tracks(video.testsrc.ffmpeg(duration=10.))
s = source({audio=audio, video=video})

s = once(s)
todo = ref(2)
done = ref(false)

def on_close(_) =
  def check(out) =
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
    ) = test.ffprobe(out)

    video_stream =
      list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)

    audio_stream =
      list.find(
        (fun (stream) -> null.defined(stream.sample_rate)),
        parsed.streams
      )

    null.get(video_stream.codec_name) == "h264"
  and
    null.get(video_stream.pix_fmt) == "yuv420p"
  and
    null.get(audio_stream.channel_layout) == "stereo"
  and
    null.get(audio_stream.codec_name) == "aac"
  and
    null.get(audio_stream.sample_fmt) == "fltp"
  and
    null.get(audio_stream.sample_rate) == "44100"
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

s =
  ffmpeg.encode.audio_video(
    id="encoder",
    %ffmpeg(%video(codec = "libx264", b = "5m"), %audio(codec = "aac")),
    s
  )

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(%video.copy, %audio.copy),
  out_copy,
  s
)

s = ffmpeg.decode.audio_video(id="decoder", s)

#clock.assign_new(sync='none', [s])
output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(%video(codec = "libx264", b = "5m"), %audio(codec = "aac")),
  out_encode,
  s
)
