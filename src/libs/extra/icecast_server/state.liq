# The state of a server started with the validated parameters `p`, passed as
# `state` to every part: `params` is the record in force, `server(hostname)` the
# `server` field of requests for that hostname, with an `instance` fixed
# for the server's life, `overlay` the run-time fallbacks and `lock` the flag
# of `icecast.server.locked`.
# @flag hidden
def icecast.server.state(p) =
  def hex(n) =
    string.concat(
      list.init(n, fun (_) -> string.hex_of_int(random.int(min=0, max=16)))
    )
  end

  name = (p.name : string)
  port = p.port
  instance = "#{hex(8)}-#{hex(4)}-4#{hex(3)}-#{hex(4)}-#{hex(12)}"
  {
    name = name,
    port = port,
    transport = p.transport,
    compatibility = p.compatibility,
    params = icecast.server.cell(p),
    server =
      fun (hostname) -> {hostname = hostname, port = port, instance = instance},
    mounts = icecast.server.mounts.table(),
    overlay = icecast.server.cell(([] : [(string * string)])),
    lock = ref(false),
    events = icecast.server.events.create(),
    counters = icecast.server.counters(),
    clock = icecast.server.cell(clock.create(id=name))
  }
end
