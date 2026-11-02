# Created once, outside `parameters`: after a reload, the mount keeps
# following the same source.
backup =
  ffmpeg.encode.audio(
    %ffmpeg(%audio(codec = "libmp3lame", sample_format = "fltp")),
    mksafe(playlist("/srv/radio/backup"))
  )

def parameters() =
  members = icecast.server.decision.htpasswd("/etc/radio/members.htpasswd")
  {
    hostname = "radio.example.org",
    source_password = "change-me",
    max_clients = 500,
    access_log = "/var/log/radio/access.log",
    mount =
      fun (path) ->
        if
          path == "/live.mp3"
        then
          {max_listeners = 200, fallback = {source = backup}}
        elsif
          path == "/members.mp3"
        then
          {listener_decision = members, fallback = {mount = "/live.mp3"}}
        else
          null
        end
  }
end

server = icecast.server(parameters())

server.on_source_connect(
  fun (r) ->
    log.important(
      "DJ connected on #{r.mount} from #{r.agent}"
    )
)

ignore(on_signal("hup", fun () -> server.reload(parameters())))
