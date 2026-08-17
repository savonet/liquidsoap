log.level.set(4)

# `source(_)` is a source whose content is unknown. Any source can be used where
# one is expected, so sources of different content can sit in the same list
# without their content types being unified into one.

audio_source = source.methods((sine(id="audio") : source(audio=pcm)))
video_source =
  source.methods((video.testsrc(id="video") : source(video=yuv420p)))

# Each element is coerced on its own: a list literal unifies its elements'
# types before the list type is checked.
sources = [(audio_source : source(_)), (video_source : source(_))]

def check() =
  audio_content = source.content(audio_source)
  video_content = source.content(video_source)
  log.important(
    "audio: #{audio_content}, video: #{video_content}"
  )

  if
    list.length(sources) == 2
    and list.assoc.mem("audio", audio_content)
    and not list.assoc.mem("video", audio_content)
    and list.assoc.mem("video", video_content)
  then
    test.pass()
  else
    test.fail()
  end
end

output.dummy(fallible=true, audio_source)
output.dummy(fallible=true, video_source)

clock.assign_new(sync="none", [audio_source])
clock.assign_new(sync="none", [video_source])

thread.run(delay=1., check)
