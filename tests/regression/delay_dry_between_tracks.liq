# Regression test: a delayed source that runs dry between tracks.
#
# `request.queue` reports the end of a track by simply running out of data: it
# never emits a track mark for it, and the next mark only shows up at position
# 0 of the following track's first frame. `delay` read that opening mark as a
# track end and re-armed on the very track it was about to play, postponing it
# by another full delay.
#
# jingle0 plays first and empties the queue. jingle1 is pushed a few seconds
# later, by which time the delay has long elapsed, so it must play at the very
# next music boundary (t=7s). Re-arming on its opening mark instead pushes it
# to the boundary after that (t=10s).

jingles = request.queue(id="jingles")
music = single("delay_music.mp3")
radio = fallback([delay(1., jingles), music])

jingles.push(request.create("delay_jingle0.mp3"))
thread.run(delay=4.5, {jingles.push(request.create("delay_jingle1.mp3"))})

radio.on_metadata(
  synchronous=true,
  fun (m) -> if m["title"] == "jingle1" then test.pass() end
)

output.dummy(fallible=true, radio)

thread.run(delay=8.5, {test.fail()})
