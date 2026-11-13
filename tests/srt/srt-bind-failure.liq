# A bind failure that may clear is retried while other outputs keep running; one
# that cannot clear fails the start (spec/connection.md §4).
%include "../test.liq"
%include "helpers.inc"

port = 19550
o =
  output.srt(
    mode="listener",
    port=port,
    polling_delay=0.3,
    start=false,
    %wav,
    sine()
  )
ticks = ref(0)
other = output.dummy(sine())
other.on_output(synchronous=true, fun () -> ticks := ticks() + 1)
listens = ref(0)
o.on_socket(
  synchronous=true,
  fun (~mode, _) -> if mode == "listen" then listens := listens() + 1 end
)

privileged =
  output.srt(
    mode="listener",
    port=80,
    polling_delay=0.3,
    start=false,
    %wav,
    sine()
  )
privileged_listens = ref(0)
privileged.on_socket(
  synchronous=true,
  fun (~mode, _) ->
    if mode == "listen" then privileged_listens := privileged_listens() + 1 end
)

def check_privileged() =
  if
    process.read(
      "id -u"
    ) ==
      "0\n"
  then
    print(
      "running as root: the privileged port check is not covered"
    )
  else
    privileged.start()
    wait_until(
      what="the privileged bind",
      {privileged_listens() >= 1}
    )
    thread.pause(1.5)
    if
      privileged_listens() != 1
    then
      test.fail(
        "a privileged port bind was retried"
      )
    end
    if
      privileged.sockets() != []
    then
      test.fail(
        "a privileged port was bound"
      )
    end
  end
end

def run() =
  released = ref(false)
  thread.run(
    fun () ->
      begin
        ignore(
          run_peer(
            "hold-udp --port #{port} --duration 5 --ready port-held"
          )
        )
        released := true
      end
  )
  wait_until(
    what="the port to be held",
    {file.exists("port-held")}
  )
  o.start()
  wait_until(
    what="retries on the held port",
    {listens() >= 3}
  )
  before = ticks()
  thread.pause(0.5)
  if
    ticks() <= before
  then
    test.fail(
      "other outputs stopped while the bind failed"
    )
  end
  wait_until(
    what="the port to be released",
    {released()}
  )
  code =
    run_peer(
      "receive --port #{port} --duration 2"
    )
  if
    code != 0
  then
    test.fail(
      "no connection once the port was freed"
    )
  end
  check_privileged()
  test.pass()
end

thread.run(delay=0.1, run)
