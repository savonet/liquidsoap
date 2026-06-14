log.level := 5

def s_ref =
  b = blank(id="first")
  b.on_collect(synchronous=true, test.pass)
  ref(b)
end

s = source.dynamic(track_sensitive=false, {s_ref()})

output.dummy(fallible=true, s)

thread.run(delay=1., fun () -> s_ref.set(blank(id="second")))

thread.run(
  delay=2.,
  fun () ->
    begin
      runtime.gc.compact()
      log(level=2, "Compacting")
    end
)

thread.run(
  delay=3.,
  fun () ->
    begin
      runtime.gc.compact()
      log(level=2, "Compacting")
    end
)
