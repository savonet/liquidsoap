log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="",1)
out = "#{fname}+ffmpeg_copy_decoder.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

#clock.assign_new(sync='none',[s])

def on_close (encoded_fname) =
  if not test.done() then
    ijson = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(fname)}")
    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(encoded_fname)}")

    let json.parse ( iparsed : {
      streams: [{
        index: int,
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_type: string,
        pix_fmt: string?
      }]
    }) = ijson

    let json.parse ( oparsed : {
      streams: [{
        index: int,
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_type: string,
        pix_fmt: string?
      }]
    }) = ojson

    filter = fun(type, l) -> list.filter(fun (s) -> s.codec_type == type, l)
    sort = fun (l) -> list.sort(fun (s1, s2) -> if s1.index < s2.index then -1 else 1 end, l)

    let [{index, ...iaudio}] = sort(filter("audio", iparsed.streams))
    let [{index, ...ivideo}] = sort(filter("video", iparsed.streams))

    let [{index, ...oaudio}] = sort(filter("audio", oparsed.streams))
    let [{index, ...ovideo}] = sort(filter("video", oparsed.streams))

    if iaudio == oaudio and ivideo == ovideo then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(fallible=true, on_close=on_close, %ffmpeg(format="mkv",%audio.copy,%video.copy), out, s)
