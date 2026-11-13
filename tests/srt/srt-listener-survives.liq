# The listener survives a rejected handshake and fifty connections, on the same
# listening socket throughout (spec/listener.md §2, §4).
%include "../test.liq"
%include "helpers.inc"

port = 19530

def admit(~hs_version:_, ~peeraddr:_, ~streamid, _) =
  streamid != "reject"
end

i = input.srt(port=port, listen_callback=admit)
output.dummy(fallible=true, i)
listening = ref([])
disconnections = ref(0)
i.on_socket(
  synchronous=true,
  fun (~mode, s) -> if mode == "listen" then listening := s.id::listening() end
)
i.on_disconnect(
  synchronous=true,
  fun () -> disconnections := disconnections() + 1
)

def run() =
  wait_until(
    what="the listener",
    {listening() != []}
  )
  if
    run_peer(
      "send --port #{port} --streamid reject"
    ) ==
      0
  then
    test.fail(
      "rejected stream id admitted"
    )
  end
  for n = 1 to 50 do
    code =
      run_peer(
        "send --port #{port} --then close"
      )
    if
      code != 0
    then
      test.fail(
        "connection #{n} failed"
      )
    end
    wait_until(
      what="disconnection #{n}",
      {disconnections() == n}
    )

  end
  if
    list.length(listening()) != 1
  then
    test.fail(
      "listening sockets: #{listening()}"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
