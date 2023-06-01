
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
  (run js_of_ocaml build-fs -I . -I ../libs -o %{target} %{stdlib_js} profiler.liq
externals.liq
io.liq
native.liq
audio.liq
http.liq
utils.liq
null.liq
source.liq
video.liq
http_codes.liq
log.liq
string.liq
replaygain.liq
process.liq
telnet.liq
getter.liq
ffmpeg.liq
http_extra.liq
deprecations.liq
socket.liq
hls.liq
switches.liq
playlist.liq
fades.liq
ref.liq
settings.liq
lastfm.liq
testing.liq
icecast.liq
interactive.liq
stdlib.liq
request.liq
list.liq
file.liq
liquidsoap.liq
medialib.liq
server.liq
metadata.liq
predicate.liq
math.liq
visualization.liq
resolvers.liq
clock.liq
thread.liq
protocols.liq
runtime.liq
error.liq
gstreamer.liq)))
