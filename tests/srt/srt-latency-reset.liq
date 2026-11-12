# After a latency reset the output is started and connecting again, in both
# roles: the peers receive data again (spec/output.md §3).
%include "../test.liq"
%include "helpers.inc"

settings.clock.max_latency := 0.5
s = sine()
caller_output =
  output.srt(host="127.0.0.1", port=19566, polling_delay=0.2, %wav, s)
listener_output = output.srt(mode="listener", port=19568, %wav, s)
i = input.srt(port=19566)
connections = ref(0)
i.on_connect(synchronous=true, fun () -> connections := connections() + 1)
output.dummy(fallible=true, i)
block = ref(false)
caller_output.on_output(
  synchronous=true,
  fun () ->
    if
      block()
    then
      block := false
      thread.pause(1.5)
    end
)

def run() =
  thread.run(
    fun () ->
      ignore(
        run_peer(
          "receive --port 19568 --duration 30"
        )
      )
  )
  wait_until(
    what="both peers",
    {connections() == 1 and listener_output.sockets() != []}
  )
  block := true
  wait_until(
    what="the caller output's reconnection",
    {connections() == 2}
  )
  wait_until(
    what="data after the reset",
    {i.is_ready()}
  )
  if
    not caller_output.is_started()
  then
    test.fail(
      "caller output stopped after the reset"
    )
  end
  code =
    run_peer(
      "receive --port 19568 --sizes after-reset.txt --duration 2"
    )
  if
    code != 0
  then
    test.fail(
      "listener output not listening after the reset"
    )
  end
  if
    payload_sizes("after-reset.txt") == []
  then
    test.fail(
      "no data from the listener output after the reset"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
