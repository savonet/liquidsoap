settings.init.force_start := true
settings.server.telnet := true

# Replace the path here with a path to some video files:
s = playlist("/path/to/files")

streams = ref([])
count = ref(0)

enc = %ffmpeg(format = "flv", %audio.copy, %video.copy)

def create_stream(url) =
  if
    list.assoc.mem(url, streams())
  then
    "Stream for url #{url} already exists!"
  else
    out = output.url(id="restream-#{count()}", fallible=true, url=url, enc, s)
    count := count() + 1
    streams := [...streams(), (url, out.shutdown)]
    "OK!"
  end
end

def delete_stream(url) =
  if
    not list.assoc.mem(url, streams())
  then
    "Stream for url #{url} does not exists!"
  else
    shutdown = list.assoc(url, streams())
    shutdown()
    streams := list.filter((fun (el) -> fst(el) != url), streams())
    "OK!"
  end
end

server.register(
  namespace="restream",
  description="Redirect a stream.",
  usage="start <url>",
  "start",
  create_stream
)
server.register(
  namespace="restream",
  description="Stop a dynamic playlist.",
  usage="stop <url>",
  "stop",
  delete_stream
)
