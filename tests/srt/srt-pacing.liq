# A connected time-controlled input paces its clock with the stream as the
# sync source: two inputs of one stream share it, two different streams in one
# clock are a conflict (spec/input.md §7).
%include "../test.liq"
%include "helpers.inc"

def run_script(~name, ~seconds, body) =
  file_name = "pacing-#{name}.liq"
  file.write(
    data="log.level := 4\n#{body}\nthread.run(delay=#{seconds}, \
     shutdown)\nthread.run(every=0.2, {if a.is_ready() and b.is_ready() then \
     thread.run(delay=2., shutdown) end})\n",
    file_name
  )
  r =
    process.run(
      timeout=60.,
      "liquidsoap #{file_name}"
    )
  (r.status.code, r.stdout ^ r.stderr)
end

senders =
  "output.srt(mode=\"listener\", port=19560, max_clients=2, %wav, \
   sine())\noutput.srt(mode=\"listener\", port=19562, %wav, sine())\n"

def caller(id, port) =
  "#{id} = input.srt(id=\"#{id}\", mode=\"caller\", host=\"127.0.0.1\", port=#{
    port
  }, polling_delay=0.2)\n"
end

def run() =
  let (code, log) =
    run_script(
      name="same",
      seconds=30.,
      senders ^ caller("a", 19560) ^ caller("b", 19560) ^
        "output.dummy(fallible=true, add([a, b]))"
    )
  if
    code != 0
  then
    test.fail(
      "one stream twice: exit #{code}"
    )
  end
  if
    string.contains(
      substring="multiple synchronization sources",
      log
    )
  then
    test.fail(
      "two inputs of one stream conflict"
    )
  end
  if
    not string.contains(
      substring="paced by sync source srt://127.0.0.1:19560",
      log
    )
  then
    test.fail(
      "the clock is not paced by the stream"
    )
  end
  let (_, log) =
    run_script(
      name="different",
      seconds=30.,
      senders ^ caller("a", 19560) ^ caller("b", 19562) ^
        "output.dummy(fallible=true, add([a, b]))"
    )
  if
    not string.contains(
      substring="multiple synchronization sources",
      log
    )
  then
    test.fail(
      "two different streams in one clock do not conflict"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
