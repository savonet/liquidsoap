%include "../test.liq"

# Two sources controlling their own latency cannot be read together: the
# default transition switches between them without mixing them.

def self_synced(frequency) =
  s = sine(frequency)
  source.dynamic(self_sync=true, infallible=true, {s})
end

music = self_synced(440.)

live_on = ref(false)
live = source.available(self_synced(880.), live_on)

s = fallback([live.{track_sensitive = false}, music])

thread.run(delay=0.5, {live_on := true})

# A clock with two sync sources fails well before the fade would be over.
thread.run(
  delay=1.5,
  {if s.selected() == live then test.pass() else test.fail() end}
)

output.dummy(fallible=true, s)
