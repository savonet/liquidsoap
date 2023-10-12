log.level.set(5)
audio.samplerate.set(48000)
video.frame.rate.set(45)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="",1)
out = "#{fname}+ffmpeg_video_decoder.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

#clock.assign_new(sync='none',[s])

def on_close (filename) =
  if not test.done() then
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

    let json.parse ( parsed: {
      streams: [{
        r_frame_rate: string,
        codec_name: string
      }]
    }) = j

    let [stream] = parsed.streams

    if stream.r_frame_rate == "45/1" and stream.codec_name == "h264" then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(fallible=true, on_close=on_close, %ffmpeg(format="mp4",%video(codec="libx264")), out, s)
