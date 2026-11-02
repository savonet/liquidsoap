%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/stats.liq"

# A mount's statistics keep peak and connections at least the listeners
# listed (observability.md §4), even for a listener listed before counted.
def f() =
  counters = icecast.server.stats.mount()
  entry =
    {
      path = "/a",
      stats = counters,
      feeder = {get = fun () -> null},
      identity = fun () -> [],
      audio = fun () -> {channels = null, samplerate = null},
      output = {last_metadata = fun () -> null},
      format = {container = null, mime = "audio/mpeg"},
      started = {get = fun () -> 0.}
    }
  stats =
    icecast.server.stats.of_mount(
      listener=fun (_, l) -> l,
      listeners=[{id = 1}],
      hidden=false,
      fallback=null,
      entry
    )
  test.equal((stats.peak_listeners, stats.connections), (1, 1))
  test.pass()
end

test.check(f)
