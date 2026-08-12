log.level.set(4)

# `source.collect_callback_releases` takes sources of unknown content, so
# sources streaming different content can be watched together without their
# content types being unified into one.

audio_source = source.methods((sine(id="audio") : source(audio=pcm)))
video_source =
  source.methods((video.testsrc(id="video") : source(video=yuv420p)))

audio_hits = ref(0)
video_hits = ref(0)

# Registering on both, over and over, the way a function called on every
# selection would.
for _ = 1 to 3 do
  let {release} =
    source.collect_callback_releases(
      [(audio_source : source(_)), (video_source : source(_))],
      fun () ->
        begin
          audio_source.on_frame(
            synchronous=true,
            before=false,
            fun () -> ref.incr(audio_hits)
          )
          video_source.on_frame(
            synchronous=true,
            before=false,
            fun () -> ref.incr(video_hits)
          )
        end
    )
  release()
end

def check() =
  audio_content = source.content(audio_source)
  video_content = source.content(video_source)
  log.important(
    "audio: #{audio_content} #{audio_hits()} hits, video: #{video_content} #{
      video_hits()
    } hits"
  )

  if
    not list.assoc.mem("audio", audio_content)
  or
    not list.assoc.mem("video", video_content)
  then
    test.fail()
  elsif
    audio_hits() > 0 or video_hits() > 0
  then
    test.fail()
  else
    test.pass()
  end
end

output.dummy(fallible=true, audio_source)
output.dummy(fallible=true, video_source)

clock.assign_new(sync="none", [audio_source])
clock.assign_new(sync="none", [video_source])

thread.run(delay=1., check)
