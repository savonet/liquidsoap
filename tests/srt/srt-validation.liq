# Invalid parameters fail at evaluation, naming the parameter; socket methods on
# a closed socket raise catchable errors; operator methods never raise
# (spec/common.md §1, spec/failures.md §7).
%include "../test.liq"
%include "helpers.inc"

def rejected(name, call) =
  file_name =
    "invalid-#{
      string.replace(
        pattern=" ",
        fun (_) -> "-",
        name
      )
    }.liq"
  file.write(data="ignore(#{call})\nshutdown()\n", file_name)
  r =
    process.run(
      timeout=120.,
      "liquidsoap #{file_name}"
    )
  output = r.stdout ^ r.stderr
  if
    r.status != "exit" or r.status.code == 0
  then
    test.fail(
      "#{name}: accepted"
    )
  elsif
    not string.contains(
      substring="Invalid value",
      output
    )
  then
    test.fail(
      "#{name}: failed without an invalid-value error: #{output}"
    )
  end
end

def check_parameters() =
  out =
    "%wav, blank()"
  rejected(
    "9-byte passphrase",
    "output.srt(passphrase=\"123456789\", #{out})"
  )
  rejected(
    "80-byte passphrase",
    "output.srt(passphrase=string.make(char_code=65, 80), #{out})"
  )
  rejected(
    "port 0",
    "output.srt(port=0, #{out})"
  )
  rejected(
    "payload size 1457",
    "output.srt(payload_size=1457, #{out})"
  )
  rejected(
    "key length 8",
    "output.srt(pbkeylen=8, #{out})"
  )
  rejected(
    "message API off",
    "output.srt(messageapi=false, #{out})"
  )
  rejected(
    "unknown mode",
    "output.srt(mode=\"rendezvous\", #{out})"
  )
  rejected(
    "unknown address family",
    "output.srt(prefer_address=\"ipv5\", #{out})"
  )
  rejected(
    "zero polling delay",
    "output.srt(polling_delay=0., #{out})"
  )
  rejected(
    "zero clients",
    "output.srt(mode=\"listener\", max_clients=0, #{out})"
  )
  rejected(
    "zero read timeout",
    "input.srt(read_timeout=0.)"
  )
end

idle = input.srt(start=false, port=19538)
output.dummy(fallible=true, idle)
idle.connect()
idle.disconnect()
if
  idle.sockets() != []
then
  test.fail(
    "an idle input has sockets"
  )
end

closed = ref(null)
i = input.srt(port=19540)
i.on_socket(
  synchronous=true,
  fun (~mode, s) -> if mode == "listen" then closed := s end
)
output.dummy(fallible=true, i)

def run() =
  check_parameters()
  wait_until(
    what="the listening socket",
    {null.defined(closed())}
  )
  s = null.get(closed())
  s.close()
  def gone() =
    try
      list.mem(s.status(), ["closed", "non_existant"])
    catch _ do
      true
    end
  end
  wait_until(
    what="the closed socket to be released",
    gone
  )
  try
    ignore(s.latency())
    test.fail(
      "an option read on a closed socket returned"
    )
  catch err do
    if
      err.kind != "srt"
    then
      test.fail(
        "closed socket error of kind #{err.kind}"
      )
    end
  end
  test.pass()
end

thread.run(delay=0.1, run)
