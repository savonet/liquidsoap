# A peer that stops reading delays neither the clock nor the other peer, and
# fails after write_timeout (spec/output.md §5).
%include "../test.liq"
%include "helpers.inc"

port = 19512
o = output.srt(mode="listener", port=port, write_timeout=1., %wav, sine())
o.on_socket(
  synchronous=true,
  fun (~mode, socket) -> if mode == "listen" then socket.set_sndbuf(65536) end
)

# WAV of a stereo 44.1 kHz s16 sine.
bytes_per_second = 176400

def run() =
  thread.run(
    fun () ->
      ignore(
        run_peer(
          "receive --port #{port} --stop-reading-after 1 --rcvbuf 65536 \
           --duration 20"
        )
      )
  )
  wait_until(
    what="the slow peer",
    {list.length(o.sockets()) == 1}
  )
  started = time()
  code =
    run_peer(
      "receive --port #{port} --bytes fast.wav --duration 6"
    )
  if
    code != 0
  then
    test.fail(
      "fast peer: no connection"
    )
  end
  elapsed = time() - started
  received = file_size("fast.wav")
  expected = int_of_float(0.7 * 5. * float(bytes_per_second))
  if
    received < expected
  then
    test.fail(
      "fast peer got #{received} bytes in #{elapsed}s, expected at least #{
        expected
      }"
    )
  end
  wait_until(
    timeout=5.,
    what="the slow peer to be dropped",
    {o.sockets() == []}
  )
  test.pass()
end

thread.run(delay=0.1, run)
