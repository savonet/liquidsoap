log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="",1)
out = "#{fname}+ffmpeg_copy_and_encode_decoder.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

#clock.assign_new(sync='none',[s])

def on_close (filename) =
  if not test.done() then
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

    let json.parse ( parsed : {
      streams: [{
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_name: string?,
        pix_fmt: string?
      }]
    }) = j

    video_stream = list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)
    audio_stream = list.find((fun (stream) -> null.defined(stream.sample_rate)), parsed.streams)

    if null.get(video_stream.codec_name) == "h264" and
       null.get(video_stream.pix_fmt) == "yuv420p" and
       null.get(audio_stream.channel_layout) == "stereo" and
       null.get(audio_stream.codec_name) == "aac" and
       null.get(audio_stream.sample_fmt) == "fltp" and
       null.get(audio_stream.sample_rate) == "44100" then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(fallible=true, on_close=on_close, %ffmpeg(format="mkv",%audio(codec="aac"),%video.copy), out, s)
