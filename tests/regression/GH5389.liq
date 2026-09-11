# An active input replaced through source.dynamic must be stopped and become
# collectable.

stopped = ref(false)
collected = ref(false)
tracks = ref(0)
home = ref(-1)

current = ref(source.fail())
dynamic =
  source.dynamic(track_sensitive=false, init=current(), fun () -> current())
output.dummy(mksafe(dynamic))

def replace() =
  current :=
    input.ffmpeg(
      id="second",
      format="lavfi",
      "sine=frequency=880:sample_rate=48000"
    )
end

def install() =
  home := runtime.domain()
  first =
    input.ffmpeg(
      id="first",
      format="lavfi",
      "sine=frequency=440:sample_rate=48000"
    )
  first.on_stop(synchronous=true, fun () -> stopped := true)
  first.on_collect(
    synchronous=true,
    fun () ->
      begin
        collected := true
        if stopped() then test.pass() else test.fail() end
      end
  )
  first.on_connect(synchronous=true, fun () -> thread.run(delay=0.2, replace))
  current := first
end

# The second track is the switch away from `first`, whose last reference is
# dropped a cycle or so later, so collection is asked for until it happens, on
# the domain that allocated it since a GC only reclaims its own domain's heap.
#
# Compacting from the clock thread would deadlock, since the finaliser passes
# the test and that shuts down the clock it is running on.
def on_track(_) =
  tracks := tracks() + 1
  if
    tracks() == 2
  then
    thread.run(
      domain=home(),
      every={(collected() ? -1. : 0.1)},
      runtime.gc.compact
    )
  end
end

dynamic.on_track(synchronous=true, on_track)

thread.run(delay=0.1, install)
