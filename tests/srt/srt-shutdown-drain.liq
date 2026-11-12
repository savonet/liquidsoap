# At shutdown, a clean end towards a peer that stopped reading is bounded even
# without write_timeout, and libsrt is still cleaned up (spec/output.md §6,
# spec/connection.md §9).
%include "../test.liq"
%include "helpers.inc"

port = 19564

child =
  "log.level := 4\no = output.srt(host=\"127.0.0.1\", port=#{port}, \
   polling_delay=0.2, write_timeout=null, %wav, \
   sine())\no.on_socket(synchronous=true, fun (~mode, socket) -> if mode == \
   \"connect\" then socket.set_sndbuf(65536) \
   end)\no.on_connect(synchronous=true, fun () -> thread.run(delay=2., fun () -> \
   begin\nfile.write(data=string(time()), \
   \"drain-shutdown-start\")\nshutdown()\nend))\nthread.run(delay=45., fun () -> \
   begin\nprint(\"no connection within 45s\")\nshutdown()\nend)\n"

def run() =
  file.write(data=child, "drain-child.liq")
  thread.run(
    fun () ->
      ignore(
        run_peer(
          "receive --listen --port #{port} --stop-reading-after 1 --rcvbuf 65536 \
           --duration 40"
        )
      )
  )
  r =
    process.run(
      timeout=60.,
      "liquidsoap drain-child.liq"
    )
  if
    r.status != "exit" or r.status.code != 0
  then
    test.fail(
      "child: #{r.status} #{r.status.code} #{r.stderr}"
    )
  end
  if
    not file.exists("drain-shutdown-start")
  then
    test.fail(
      "the child never connected: #{r.stdout}"
    )
  end
  elapsed = time() - float_of_string(file.contents("drain-shutdown-start"))
  child_log = r.stdout ^ r.stderr
  if
    string.contains(
      substring="still in use",
      child_log
    )
  then
    test.fail(
      "libsrt cleanup skipped after #{elapsed}s: a thread outlived the drain \
       limit"
    )
  end
  if
    not string.contains(
      substring="libsrt cleaned up",
      child_log
    )
  then
    test.fail(
      "no libsrt cleanup in the child's log"
    )
  end
  if
    elapsed > 5.
  then
    test.fail(
      "shutdown took #{elapsed}s towards a stalled peer"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
