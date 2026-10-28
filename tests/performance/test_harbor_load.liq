%include "../test.liq"

# Thousands of listeners on one output, a few that never read: the clock keeps
# real time, memory per listener is bounded and does not grow with stream
# length, every reader gets its first byte quickly, and the non-readers are
# disconnected.
port = 18128
readers = 2000
stalled = 5
duration = 90.

o =
  output.harbor(
    port=port,
    mount="load.mp3",
    timeout=5.,
    %mp3(bitrate = 128),
    sine()
  )

stalled_closed = ref(0)
o.on_disconnect(
  synchronous=true,
  fun (l) ->
    if
      list.assoc(default="", "user-agent", l.headers) == "stalled"
    then
      atomic(fun () -> stalled_closed := stalled_closed() + 1)
    end
)

# Bytes the socket accepted for listeners that read.
def readers_sent() =
  list.fold(
    fun (total, l) ->
      if
        list.assoc(default="", "user-agent", l.headers) == "reader"
      then
        total + l.bytes_sent
      else
        total
      end,
    0,
    o.listeners()
  )
end

def memory() =
  runtime.memory().process_physical_memory
end

def check() =
  memory_idle = memory()
  report = ref(null)
  thread.run(
    fun () ->
      report :=
        (
          process.run(
            timeout=duration + 60.,
            "./harbor_load_client.exe #{port} load.mp3 #{readers} #{stalled} #{
              duration
            }"
          ).stdout
        :
          string?
        )
  )
  thread.pause(15.)
  connected = list.length(o.listeners())
  memory_early = memory()
  ticks_early = clock(o.clock).ticks()
  sent_early = readers_sent()
  start = time()
  thread.pause(60.)
  ticks_late = clock(o.clock).ticks()
  rate = float(readers_sent() - sent_early) / float(readers) / (time() - start)
  elapsed = time() - start
  memory_late = memory()

  def rec wait(n) =
    if
      not null.defined(report())
    then
      if
        n == 0
      then
        test.fail(
          "load client did not finish"
        )
      end
      thread.pause(1.)
      wait(n - 1)
    end
  end
  wait(120)
  summary =
    list.find(
      default="",
      fun (line) ->
        string.contains(
          prefix="LOAD ",
          line
        ),
      string.split(separator="\n", null.get(report()))
    )
  fields =
    list.map(
      fun (field) ->
        begin
          let [name, value] = string.split(separator="=", field)
          (name, value)
        end,
      list.tl(
        string.split(
          separator=" ",
          summary
        )
      )
    )
  field = fun (name) -> list.assoc(default="nan", name, fields)
  expected_ticks = elapsed / settings.frame.duration()
  per_listener = (memory_early - memory_idle) / readers
  growth = memory_late - memory_early
  print(
    "load: #{connected} listeners, ticks #{ticks_late - ticks_early} for #{
      expected_ticks
    } expected, #{per_listener} bytes per listener, growth #{growth} bytes over \
     60s, stalled closed #{stalled_closed()}, reader rate #{rate} B/s, #{
      summary
    }"
  )
  if
    connected < readers
  then
    test.fail(
      "only #{connected} listeners"
    )
  end
  if
    float(ticks_late - ticks_early) < 0.95 * expected_ticks
  then
    test.fail(
      "clock fell behind"
    )
  end
  if
    per_listener > 200000
  then
    test.fail(
      "memory per listener"
    )
  end
  if
    growth > 50000000
  then
    test.fail(
      "memory grows with stream length"
    )
  end
  if
    int_of_string(field("connected")) != readers
  then
    test.fail(
      "readers without a first byte"
    )
  end
  if
    int_of_string(field("closed_early")) != 0
  then
    test.fail(
      "readers closed early"
    )
  end
  if
    float_of_string(field("ttfb_p99")) > 1.
  then
    test.fail(
      "slow first byte"
    )
  end
  if
    rate < 0.9 * 16000.
  then
    test.fail(
      "readers fell behind the stream"
    )
  end
  if
    stalled_closed() != stalled
  then
    test.fail(
      "stalled listeners not closed"
    )
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=2., check))
