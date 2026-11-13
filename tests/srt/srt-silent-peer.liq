# A connected peer that sends nothing leaves the input not ready and keeps its
# connection; the clock ticks, and stopping does not wait for the peer
# (spec/input.md §3, §4).
%include "../test.liq"
%include "helpers.inc"

ticks = ref(0)

def silent_input(~port, ~read_timeout) =
  i = input.srt(port=port, read_timeout=read_timeout)
  disconnections = ref(0)
  i.on_disconnect(
    synchronous=true,
    fun () -> disconnections := disconnections() + 1
  )
  o = output.dummy(fallback([i, blank()]))
  o.on_output(synchronous=true, fun () -> ticks := ticks() + 1)
  thread.run(
    fun () ->
      ignore(
        run_peer(
          "send --port #{port} --then silent --duration 30"
        )
      )
  )
  (i, disconnections)
end

inputs =
  [
    ("0.5s", silent_input(port=19514, read_timeout=0.5)),
    (
      "under 1ms",
      silent_input(port=19516, read_timeout=0.0001)
    ),
    ("null", silent_input(port=19518, read_timeout=null))
  ]

def run() =
  list.iter(
    fun (entry) ->
      begin
        let (name, (i, _)) = entry
        wait_until(
          what="#{name}: a connected peer",
          {list.length(i.sockets()) == 1}
        )
      end,
    inputs
  )
  before = ticks()
  thread.pause(2.)
  if
    ticks() <= before
  then
    test.fail(
      "the clock stopped ticking"
    )
  end
  list.iter(
    fun (entry) ->
      begin
        let (name, (i, disconnections)) = entry
        if
          i.is_ready()
        then
          test.fail(
            "#{name}: ready without data"
          )
        end
        if
          list.length(i.sockets()) != 1 or disconnections() != 0
        then
          test.fail(
            "#{name}: the silent peer was disconnected"
          )
        end
        i.stop()
        wait_until(
          timeout=2.,
          what="#{name}: stop",
          {i.sockets() == []}
        )
      end,
    inputs
  )
  test.pass()
end

thread.run(delay=0.1, run)
