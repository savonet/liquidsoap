# Events pair per peer: on_connect then on_disconnect, once each, a failed
# attempt fires neither, and the input's dump is open in on_connect
# (spec/connection.md §5, §8).
%include "../test.liq"
%include "helpers.inc"

port = 19526
o =
  output.srt(
    port=port,
    host="127.0.0.1",
    polling_delay=0.2,
    connection_timeout=0.5,
    %wav,
    sine()
  )
i = input.srt(port=port, start=false, dump="pairing.wav")
output.dummy(fallible=true, i)
output_events = ref([])
input_events = ref([])
attempts = ref(0)
o.on_socket(
  synchronous=true,
  fun (~mode, _) -> if mode == "connect" then attempts := attempts() + 1 end
)
o.on_connect(
  synchronous=true,
  fun () -> output_events := "connect"::output_events()
)
o.on_disconnect(
  synchronous=true,
  fun () -> output_events := "disconnect"::output_events()
)
i.on_connect(
  synchronous=true,
  fun () ->
    begin
      if
        not file.exists("pairing.wav")
      then
        test.fail(
          "dump not open in on_connect"
        )
      end
      input_events := "connect"::input_events()
    end
)
i.on_disconnect(
  synchronous=true,
  fun () -> input_events := "disconnect"::input_events()
)

def alternates(events) =
  def rec check(expected, l) =
    if
      l == []
    then
      true
    elsif
      list.hd(l) != expected
    then
      false
    else
      check(expected == "connect" ? "disconnect" : "connect", list.tl(l))
    end
  end
  check("connect", list.rev(events))
end

def run() =
  wait_until(
    what="three failed attempts",
    {attempts() >= 3}
  )
  if
    output_events() != []
  then
    test.fail(
      "a failed attempt fired #{output_events()}"
    )
  end
  i.start()
  wait_until(
    what="connection 1",
    {list.length(input_events()) == 1}
  )
  for n = 1 to 2 do
    i.disconnect()
    wait_until(
      what="disconnection #{n}",
      {list.length(output_events()) == 2 * n}
    )
    i.connect()
    wait_until(
      what="connection #{n + 1}",
      {list.length(output_events()) == 2 * n + 1}
    )

  end
  if
    not alternates(output_events())
  then
    test.fail(
      "output events #{output_events()}"
    )
  end
  if
    not alternates(input_events())
  then
    test.fail(
      "input events #{input_events()}"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
