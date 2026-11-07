# One listener keeps accepting while another is stopped and started, with
# callers arriving all along (spec/poll.md §3).
%include "../test.liq"
%include "helpers.inc"

steady = output.srt(mode="listener", port=19586, %wav, sine())
toggled = output.srt(mode="listener", port=19588, %wav, sine())
running = ref(true)
accepted = ref(0)
steady.on_connect(synchronous=true, fun () -> accepted := accepted() + 1)

def rec toggle() =
  if
    running()
  then
    toggled.stop()
    thread.pause(0.05)
    toggled.start()
    thread.pause(0.05)
    toggle()
  end
end

def run() =
  thread.run(toggle)
  for batch = 1 to 4 do
    codes = ref([])
    for _ = 1 to 10 do
      thread.run(
        fun () ->
          begin
            code =
              run_peer(
                "receive --port 19586 --duration 0.5"
              )
            atomic({codes := code::codes()})
          end
      )
      thread.run(
        fun () ->
          ignore(
            run_peer(
              "receive --port 19588 --duration 0.3"
            )
          )
      )

    end
    wait_until(
      timeout=30.,
      what="batch #{batch}",
      {list.length(codes()) == 10}
    )
    failed = list.filter(fun (c) -> c != 0, codes())
    if
      failed != []
    then
      test.fail(
        "batch #{batch}: #{list.length(failed)} callers refused"
      )
    end
    wait_until(
      what="batch #{batch} accepted",
      {accepted() == 10 * batch}
    )

  end
  running := false
  test.pass()
end

thread.run(delay=0.1, run)
