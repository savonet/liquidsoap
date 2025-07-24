out_file = file.temp(cleanup=true, "output", ".mp4")

audio = once(single("../media/@shine[channels=2].mp3"))

video =
  single(
    "../media/@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"
  )

stream = source(source.tracks(audio).{video=source.tracks(video).video})

clock.assign_new(sync='none', [stream])

enc = %ffmpeg(%audio.copy, %video.copy)

def on_stop() =
  j =
    process.read(
      "ffprobe -v quiet -print_format json -show_streams #{out_file}"
    )

  let json.parse ({streams} : {streams: [{codec_name: string}]}) = j
  if
    not list.exists(fun (s) -> s.codec_name == "mp3", streams)
  then
    test.fail()
  end
  if
    not list.exists(fun (s) -> s.codec_name == "h264", streams)
  then
    test.fail()
  end
  test.pass()
end

o = output.file(fallible=true, enc, out_file, stream)
o.on_stop(synchronous=true, on_stop)
