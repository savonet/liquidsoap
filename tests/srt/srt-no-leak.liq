# Failures at every setup step, repeated, close every socket exactly once with
# its close event and return the descriptors to their baseline; dropped
# operators are collected (spec/connection.md §3, spec/library.md §4).
%include "../test.liq"
%include "helpers.inc"

if
  file.ls("/proc/self/fd") == []
then
  print(
    "no /proc/self/fd: descriptor leaks not covered"
  )
  test.skip()
end

def descriptors() =
  list.length(file.ls("/proc/self/fd"))
end

created = ref(0)
closed = ref(0)

def count(x) =
  x.on_socket(
    synchronous=true,
    fun (~mode, _) ->
      atomic(
        {
          if
            mode == "close"
          then
            closed := closed() + 1
          else
            created := created() + 1
          end
        }
      )
  )
end

def reject(~hs_version:_, ~peeraddr:_, ~streamid, _) =
  streamid != "reject"
end

listener = input.srt(port=19572, listen_callback=reject, start=false)
output.dummy(fallible=true, listener)
count(listener)
dead =
  output.srt(
    host="127.0.0.1",
    port=19574,
    connection_timeout=0.1,
    polling_delay=0.05,
    start=false,
    %wav,
    sine()
  )
count(dead)
rejected =
  output.srt(
    host="127.0.0.1",
    port=19572,
    streamid="reject",
    polling_delay=0.05,
    start=false,
    %wav,
    sine()
  )
count(rejected)
raising =
  output.srt(
    host="127.0.0.1",
    port=19572,
    polling_delay=0.05,
    start=false,
    %wav,
    sine()
  )
count(raising)
raising.on_socket(
  synchronous=true,
  fun (~mode:_, _) -> error.raise(error.not_found, "injected")
)

def settled(what) =
  wait_until(
    timeout=20.,
    what="#{what}: every socket closed",
    {created() == closed()}
  )
end

def phases() =
  for _ = 1 to 20 do
    dead.start()
    thread.pause(0.15)
    dead.stop()

  end
  settled(
    "unreachable caller"
  )
  listener.start()
  rejected.start()
  thread.pause(2.)
  rejected.stop()
  raising.start()
  thread.pause(1.)
  raising.stop()
  for _ = 1 to 20 do
    raising.start()
    thread.pause(random.float(min=0., max=0.1))
    listener.stop()
    listener.start()
    raising.stop()

  end
  listener.stop()
  settled(
    "rejected, raising and interrupted connections"
  )
end

def live_words() =
  runtime.gc.full_major()
  runtime.gc.stat().live_words
end

def run() =
  phases()
  baseline = descriptors()
  phases()
  wait_until(
    timeout=20.,
    what="descriptors back to #{baseline}",
    {descriptors() <= baseline}
  )
  before = live_words()
  for _ = 1 to 200 do ignore(input.srt(port=19578, start=false))  end
  after = live_words()
  if
    after > before + 2000000
  then
    test.fail(
      "200 dropped operators kept #{after - before} words alive"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
