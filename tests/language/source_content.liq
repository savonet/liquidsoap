def f() =
  # Test source.content on an audio-only source
  audio_content = source.content(sine())
  test.equal(list.length(audio_content), 1)
  let [(field, fmt)] = audio_content
  test.equal(field, "audio")

  # Check format.description returns a pcm record
  desc = format.description(fmt)
  test.equal(null.defined(desc?.pcm), true)
  test.equal((null.get(desc?.pcm)).channels, 2)
  test.equal((null.get(desc?.pcm)).channel_layout, "stereo")

  # Test source.content and track.format on a source with both audio and video
  s = (noise() : source(audio=pcm, video=yuv420p))
  av_content = source.content(s)
  audio_fmt = list.assoc("audio", av_content)
  video_fmt = list.assoc("video", av_content)

  # Audio part should be pcm
  audio_desc = format.description(audio_fmt)
  test.equal(null.defined(audio_desc?.pcm), true)
  test.equal((null.get(audio_desc?.pcm)).channels, 2)

  # Video part should be yuv420p
  video_desc = format.description(video_fmt)
  test.equal(null.defined(video_desc?.yuv420p), true)
  test.equal(
    (null.get(video_desc?.yuv420p)).width,
    settings.frame.video.width()
  )
  test.equal(
    (null.get(video_desc?.yuv420p)).height,
    settings.frame.video.height()
  )

  # Test track.format gives the same result as source.content
  let {audio, video} = source.tracks(s)
  test.equal(null.defined(format.description(track.format(audio))?.pcm), true)
  test.equal(
    null.defined(format.description(track.format(video))?.yuv420p),
    true
  )

  # Test JSON serialization of content_format via json.stringify
  test.equal(
    json.stringify(compact=true, audio_fmt),
    '{"pcm":{"channel_layout":"stereo","channels":2}}'
  )
  test.equal(
    json.stringify(compact=true, video_fmt),
    '{"yuv420p":{"height":#{settings.frame.video.height()},"width":#{
      settings.frame.video.width()
    }}}'
  )

  test.pass()
end

test.check(f)
