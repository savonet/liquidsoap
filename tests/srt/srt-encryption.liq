# Encryption is enforced: the same passphrase carries data, a wrong or missing
# one gives no peer (spec/common.md §2).
%include "../test.liq"
%include "helpers.inc"

port = 19534
i =
  input.srt(
    port=port,
    passphrase="correct horse battery",
    dump="encrypted.ts"
  )
output.dummy(fallible=true, i)
disconnections = ref(0)
i.on_disconnect(
  synchronous=true,
  fun () -> disconnections := disconnections() + 1
)

def run() =
  if
    run_peer(
      "send --port #{port} --passphrase 'wrong horse battery'"
    ) ==
      0
  then
    test.fail(
      "a wrong passphrase connected"
    )
  end
  if
    run_peer(
      "send --port #{port}"
    ) ==
      0
  then
    test.fail(
      "no passphrase connected"
    )
  end
  code =
    run_peer(
      "send --port #{port} --passphrase 'correct horse battery' --file tone.ts"
    )
  if
    code != 0
  then
    test.fail(
      "the right passphrase did not connect"
    )
  end
  wait_until(
    what="the end of the stream",
    {disconnections() == 1}
  )
  if
    file.contents("encrypted.ts") != file.contents("tone.ts")
  then
    test.fail(
      "the encrypted stream did not arrive whole"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
