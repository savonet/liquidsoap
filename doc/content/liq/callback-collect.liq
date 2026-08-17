settings.init.force_start := true
settings.server.telnet := true

s = playlist("/path/to/files")

streams = ref([])
count = ref(0)

enc = %mp3

# BEGIN
def create_stream(url) =
  if
    list.assoc.mem(url, streams())
  then
    "Stream for url #{url} already exists!"
  else
    # `fade.in` registers callbacks on `s`, which lives for as long as the
    # script does.
    stream =
      source.collect_callback_releases(
        [s],
        {
          output.url(
            id="restream-#{count()}",
            fallible=true,
            url=url,
            enc,
            fade.in(duration=2., s)
          )
        }
      )

    def stop() =
      stream.result.shutdown()
      stream.release()
    end

    count := count() + 1
    streams := [...streams(), (url, stop)]
    "OK!"
  end
end

# END

def delete_stream(url) =
  if
    not list.assoc.mem(url, streams())
  then
    "Stream for url #{url} does not exists!"
  else
    stop = list.assoc(url, streams())
    stop()
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
