# The state of a server started with the validated parameters `p`, passed as
# `state` to every part: `params` is the record in force, `server(hostname)`
# the `server` field of requests for that hostname, with an `instance` fixed
# for the server's life, `overlay` the run-time fallbacks, `logs` the log
# files (logs.liq) and `lock` the flag of `icecast.server.locked`.
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
  params = icecast.server.cell(p)
  {
    name = name,
    port = port,
    transport = p.transport,
    compatibility = p.compatibility,
    params = params,
    logs =
      icecast.server.logs.create(
        label=name,
        compatibility=p.compatibility,
        fun () -> icecast.server.logs.settings(params.get())
      ),
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
