%include "../test.liq"

# The libx264 default tag in mp4 is avc1: each step expects avc3.
avc1_file =
  "../media/@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"

encoded_file = file.temp(cleanup=true, "encoded", ".mp4")
kept_file = file.temp(cleanup=true, "kept", ".mp4")
override_file = file.temp(cleanup=true, "override", ".mp4")
integer_file = file.temp(cleanup=true, "integer", ".mp4")

# A printed format parses back to the same format.
def round_trip(enc) =
  printed = string(enc)
  let eval (reprinted : string) = "string(#{printed})"
  if
    reprinted != printed
  then
    print(
      "#{printed} prints back as #{reprinted}"
    )
    test.fail()
  end
end

round_trip(%ffmpeg(%video.copy(ignore_keyframe, codec_tag = "hvc1")))
round_trip(%ffmpeg(%video(codec = "libx264", codec_tag = "avc3")))

def video_tag(probed_file) =
  let json.parse ({streams} :
    {streams: [{codec_type: string, codec_tag_string: string}]}
  ) = test.ffprobe(probed_file)
  let [stream] = list.filter(fun (s) -> s.codec_type == "video", streams)
  stream.codec_tag_string
end

def remux(label, enc, source_file, (target_file:string), next) =
  s = once(single(source_file))
  clock.assign_new(sync="none", [s])
  o = output.file(fallible=true, enc, target_file, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        tag = video_tag(target_file)
        if
          tag != "avc3"
        then
          print(
            "#{label}: expected codec tag avc3, got #{tag}"
          )
          test.fail()
        end
        next()
      end
  )
end

# libavcodec's own integer form of the tag.
def integer() =
  remux(
    "integer",
    %ffmpeg(
      format = "mp4",
      %audio.copy,
      %video(codec = "libx264", codec_tag = 862156385)
    ),
    avc1_file,
    integer_file,
    test.pass
  )
end

def override() =
  remux(
    "override",
    %ffmpeg(format = "mp4", %audio.copy, %video.copy(codec_tag = "avc3")),
    avc1_file,
    override_file,
    integer
  )
end

def kept() =
  remux(
    "kept",
    %ffmpeg(format = "mp4", %audio.copy, %video.copy),
    encoded_file,
    kept_file,
    override
  )
end

remux(
  "encoded",
  %ffmpeg(
    format = "mp4",
    %audio.copy,
    %video(codec = "libx264", codec_tag = "avc3")
  ),
  avc1_file,
  encoded_file,
  kept
)
