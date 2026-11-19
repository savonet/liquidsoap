%include "../test.liq"

# Decoder options (decoder.md §4.4): prefixed keys go to the streams'
# decoders, an option every decoder of its media type rejects is an error,
# options for a missing media type are ignored.

h264_file = argv(default="", 1)
audio_only_file = argv(default="", 2)
h264_with_cover_file = argv(default="", 3)

audio_video = (blank() : source(audio=pcm(stereo), video=yuv420p))
audio_only = (blank() : source(audio=pcm(stereo)))
video_only = (blank() : source(video=yuv420p))

def resolves(content_type, options, fname) =
  request.resolve(
    content_type=content_type,
    request.create("annotate:ffmpeg_options='#{options}':#{fname}")
  )
end

def input_ffmpeg_accepts(string_args) =
  try
    ignore(input.ffmpeg(string_args=string_args, h264_file))
    true
  catch _ do
    false
  end
end

cases =
  [
    (
      "low_delay is accepted on H.264",
      resolves(audio_video, '"video.flags"="low_delay"', h264_file),
      true
    ),
    (
      "an unknown decoder option is rejected",
      resolves(audio_video, '"video.nosuchoption"=1', h264_file),
      false
    ),
    (
      "an invalid value is rejected",
      resolves(audio_video, '"video.flags"="bogus"', h264_file),
      false
    ),
    (
      "a video option on an audio-only file is ignored",
      resolves(audio_only, '"video.flags"="low_delay"', audio_only_file),
      true
    ),
    (
      "a prefix without an option name is rejected",
      resolves(audio_video, '"video."=1', h264_file),
      false
    ),
    (
      "an H.264 option is accepted next to a PNG cover",
      resolves(video_only, '"video.enable_er"=1', h264_with_cover_file),
      true
    ),
    (
      "input.ffmpeg takes a decoder option",
      input_ffmpeg_accepts([("video.flags", "low_delay")]),
      true
    ),
    (
      "input.ffmpeg rejects an unknown decoder option when evaluated",
      input_ffmpeg_accepts([("video.nosuchoption", "1")]),
      false
    ),
    (
      "input.ffmpeg rejects a prefix without an option name when evaluated",
      input_ffmpeg_accepts([("video.", "1")]),
      false
    )
  ]

failures =
  list.filter(
    fun (c) ->
      begin
        let (name, result, expected) = c
        if
          result != expected
        then
          print(
            "Failed: #{name}"
          )
        end
        result != expected
      end,
    cases
  )

if
  list.length(cases) == 9 and failures == []
then
  test.pass()
else
  test.fail()
end
