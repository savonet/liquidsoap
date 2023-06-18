
(install
 (section
  (site
   (liquidsoap-lang libs)))
 (package liquidsoap-libs)
 (files
    audio.liq
clock.liq
deprecations.liq
error.liq
fades.liq
ffmpeg.liq
file.liq
getter.liq
hls.liq
http.liq
http_codes.liq
icecast.liq
io.liq
liquidsoap.liq
list.liq
log.liq
math.liq
medialib.liq
metadata.liq
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
testing.liq
thread.liq
utils.liq
video.liq))

(install
 (section
  (site
   (liquidsoap-lang libs)))
 (package liquidsoap-libs-extra)
 (files
    extra/audio.liq
extra/externals.liq
extra/file.liq
extra/gstreamer.liq
extra/http.liq
extra/interactive.liq
extra/lastfm.liq
extra/native.liq
extra/server.liq
extra/source.liq
extra/telnet.liq
extra/visualization.liq))
