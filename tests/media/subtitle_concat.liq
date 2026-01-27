log.level.set(4)

let {video} = source.tracks(video.testsrc.ffmpeg(duration=32.))

let {subtitle = m1} = source.tracks((single("test-subtitle.srt"):source(audio=none,video=none,subtitle=subtitle(text))))
let {subtitle = m2} = source.tracks((single("test-subtitle.srt"):source(audio=none,video=none,subtitle=subtitle(text))))

subtitle_source = sequence([source({subtitle=m1}), source({subtitle=m2})])
let {subtitle} = source.tracks(subtitle_source)

s = source({video=video, subtitle=subtitle})

s = once(s)

clock.assign_new(sync="none",[s])

output.file(
  fallible=true,on_close=fun (_) -> test.pass(),
  reopen_on_error=fun (_) -> begin test.fail(); null end,
  %ffmpeg(format="matroska",%video(codec="libx264"),%subtitle(codec="subrip")),
  "subtitle_concat.mkv",s)
