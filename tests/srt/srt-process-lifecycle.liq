# libsrt's process lifecycle: --version, --check with and without SRT, an
# interrupt while streaming and a clean stop all exit with status 0
# (spec/library.md §1).
%include "../test.liq"
%include "helpers.inc"

def exits_cleanly(~what, command) =
  r = process.run(timeout=120., command)
  if
    r.status != "exit" or r.status.code != 0
  then
    test.fail(
      "#{what}: #{r.status} #{r.status.code} #{r.stderr}"
    )
  end
end

streaming =
  "o = output.srt(mode=\"listener\", port=19580, %wav, sine())\ni = \
   input.srt(mode=\"caller\", host=\"127.0.0.1\", port=19580, \
   polling_delay=0.2)\noutput.dummy(fallible=true, i)\n"

def run() =
  file.write(
    data=streaming ^
      "thread.run(delay=3., shutdown)\n",
    "lifecycle-stop.liq"
  )
  file.write(
    data=streaming ^
      "i.on_connect(synchronous=true, fun () -> file.write(data=\"\", \
       \"lifecycle-streaming\"))\n",
    "lifecycle-interrupt.liq"
  )
  file.write(data="output.dummy(sine())\n", "lifecycle-no-srt.liq")
  file.write(
    data="for _ = 1 to 5 do\noutput.srt(host=\"192.0.2.1\", port=19594, \
     connection_timeout=10., %wav, sine())\nend\nthread.run(delay=2., fun () -> \
     begin\nfile.write(data=string(time()), \
     \"lifecycle-shutdown-start\")\nshutdown()\nend)\n",
    "lifecycle-dead-callers.liq"
  )
  exits_cleanly(
    what="--version",
    "liquidsoap --version"
  )
  exits_cleanly(
    what="--check with SRT",
    "liquidsoap --check lifecycle-stop.liq"
  )
  exits_cleanly(
    what="--check without SRT",
    "liquidsoap --check lifecycle-no-srt.liq"
  )
  exits_cleanly(
    what="clean stop",
    "liquidsoap lifecycle-stop.liq"
  )
  exits_cleanly(
    what="interrupt while streaming",
    "liquidsoap lifecycle-interrupt.liq & pid=$!; while [ ! -e \
     lifecycle-streaming ]; do sleep 0.1; done; kill -INT $pid; wait $pid"
  )
  exits_cleanly(
    what="shutdown with attempts in flight",
    "liquidsoap lifecycle-dead-callers.liq"
  )
  elapsed = time() - float_of_string(file.contents("lifecycle-shutdown-start"))
  if
    elapsed > 3.
  then
    test.fail(
      "shutdown took #{elapsed}s with attempts in flight"
    )
  end
  print(
    "no daemon mode in this build: daemonised run not covered"
  )
  test.pass()
end

thread.run(delay=0.1, run)
