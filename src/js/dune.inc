
(rule
  (target filesystem.js)
  (enabled_if (not %{bin-available:js_of_ocaml}))
  (action (write-file %{target} "console.log(\"no js_of_ocaml available!\")")))

(rule
 (target filesystem.js)
 (enabled_if %{bin-available:js_of_ocaml})
 (deps
  (:stdlib_js ./stdlib_js.liq)
  (source_tree ../libs))
 (action
  (run js_of_ocaml build-fs -I . -I ../libs -o %{target} %{stdlib_js} audio.liq
clock.liq
deprecations.liq
error.liq
externals.liq
fades.liq
ffmpeg.liq
file.liq
getter.liq
gstreamer.liq
hls.liq
http.liq
http_codes.liq
http_extra.liq
icecast.liq
interactive.liq
io.liq
lastfm.liq
liquidsoap.liq
list.liq
log.liq
math.liq
medialib.liq
metadata.liq
native.liq
null.liq
playlist.liq
predicate.liq
process.liq
profiler.liq
protocols.liq
ref.liq
replaygain.liq
request.liq
resolvers.liq
runtime.liq
server.liq
settings.liq
socket.liq
source.liq
stdlib.liq
string.liq
switches.liq
telnet.liq
testing.liq
thread.liq
utils.liq
video.liq
visualization.liq)))
