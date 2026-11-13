# Callers trying an unreachable address hold neither the clock nor a shared
# worker: a local output keeps its pace and a server request is answered
# (spec/concurrency.md §1).
%include "../test.liq"
%include "helpers.inc"

s = sine()
for _ = 1 to 20 do
  output.srt(
    host="192.0.2.1",
    port=19582,
    connection_timeout=3.,
    polling_delay=0.1,
    %wav,
    s
  )

end
ticks = ref(0)
local = output.dummy(s)
local.on_output(synchronous=true, fun () -> ticks := ticks() + 1)

def handler(_, response) =
  response.data("pong")
end

harbor.http.register("/ping", port=19584, handler)

def run() =
  thread.pause(1.)
  start = time()
  before = ticks()
  for _ = 1 to 10 do
    t = time()
    r = http.get(timeout=5., "http://127.0.0.1:19584/ping")
    if
      "#{r}" != "pong"
    then
      test.fail(
        "server request failed: #{r.status_code}"
      )
    end
    if
      time() - t > 1.
    then
      test.fail(
        "server request took #{time() - t}s"
      )
    end
    thread.pause(0.3)

  end
  elapsed = time() - start
  frames = ticks() - before
  expected = int_of_float(elapsed / settings.frame.duration())
  if
    frames < expected * 8 / 10
  then
    test.fail(
      "#{frames} frames in #{elapsed}s, expected about #{expected}"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
