music = playlist("~/Music")

def print_title(m) =
  log(
    "Now playing: #{m["title"]}"
  )
end

# Registers on the source it is given, so each call leaves one more callback on
# that source.
def announce(s) =
  s.on_metadata(synchronous=true, print_title)
end

announcement = ref(null)

def start_announcing() =
  if
    null.defined(announcement())
  then
    "already announcing"
  else
    announcement := announce(music)
    "announcing"
  end
end

def stop_announcing() =
  if null.defined(announcement()) then null.get(announcement()).release() end
  announcement := null
  "stopped"
end

server.register("announce.start", fun (_) -> start_announcing())
server.register("announce.stop", fun (_) -> stop_announcing())
