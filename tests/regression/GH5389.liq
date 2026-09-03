# An active input replaced through source.dynamic must be stopped and become
# collectable.

stopped = ref(false)

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
  first =
    input.ffmpeg(
      id="first",
      format="lavfi",
      "sine=frequency=440:sample_rate=48000"
    )
  first.on_stop(synchronous=true, fun () -> stopped := true)
  first.on_collect(
    synchronous=true,
    fun () -> if stopped() then test.pass() else test.fail() end
  )
  first.on_connect(synchronous=true, fun () -> thread.run(delay=0.2, replace))
  current := first
end

thread.run(delay=0.1, install)
thread.run(delay=2., runtime.gc.compact)
thread.run(delay=4., test.fail)
