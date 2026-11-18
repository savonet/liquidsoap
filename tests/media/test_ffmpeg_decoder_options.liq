%include "../test.liq"

# Decoder options (decoder.md §4.4): prefixed keys reach the stream's decoder,
# unknown ones are rejected, options for a missing media type are ignored.

h264_file = argv(default="", 1)
audio_only_file = argv(default="", 2)

audio_video = (blank() : source(audio=pcm(stereo), video=yuv420p))
audio_only = (blank() : source(audio=pcm(stereo)))

def resolves(content_type, options, fname) =
  request.resolve(
    content_type=content_type,
    request.create("annotate:ffmpeg_options='#{options}':#{fname}")
  )
end

cases =
  [
    (
      "low_delay reaches the H.264 decoder",
      resolves(audio_video, '"video.flags"="low_delay"', h264_file),
      true
    ),
    (
      "an unknown decoder option is rejected",
      resolves(audio_video, '"video.nosuchoption"=1', h264_file),
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
  list.length(cases) == 4 and failures == []
then
  test.pass()
else
  test.fail()
end
