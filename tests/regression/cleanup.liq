log.level := 5

collected = ref(false)

# An object is collected only by a GC on the domain that allocated it, so the
# source is created from a task and everything after runs pinned to that
# task's domain, whichever worker took it.
def setup() =
  home = runtime.domain()
  b = blank(id="first")
  b.on_collect(
    synchronous=true,
    fun () ->
      begin
        collected := true
        test.pass()
      end
  )
  s_ref = ref(b)
  s = source.dynamic(track_sensitive=false, {s_ref()})
  ignore(output.dummy(fallible=true, s))
  thread.run(domain=home, delay=1., fun () -> s_ref.set(blank(id="second")))
  thread.run(
    domain=home,
    delay=2.,
    every={collected() ? -1. : 0.2},
    runtime.gc.full_major
  )
end

thread.run(setup)
