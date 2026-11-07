# One listener client failing leaves the other's stream intact: one continuous
# stream with one header (spec/output.md §5).
%include "../test.liq"
%include "helpers.inc"

port = 19508
o = output.srt(mode="listener", port=port, %ogg(%vorbis), sine())

def run() =
  survivor_done = ref(false)
  thread.run(
    fun () ->
      begin
        code =
          run_peer(
            "receive --port #{port} --bytes survivor.ogg --duration 12"
          )
        if
          code != 0
        then
          test.fail(
            "survivor: no connection"
          )
        end
        survivor_done := true
      end
  )
  wait_until(
    what="the survivor",
    {list.length(o.sockets()) == 1}
  )
  thread.pause(1.)
  code =
    run_peer(
      "receive --port #{port} --bytes leaver.ogg --duration 2"
    )
  if
    code != 0
  then
    test.fail(
      "leaver: no connection"
    )
  end
  wait_until(
    what="the leaver to be dropped",
    {list.length(o.sockets()) == 1}
  )
  wait_until(
    timeout=30.,
    what="the survivor to finish",
    {survivor_done()}
  )
  data = file.contents("survivor.ogg")
  starts = ogg_stream_starts(data)
  if
    starts != 1
  then
    test.fail(
      "survivor saw #{starts} stream headers"
    )
  end
  check_decodes(what="survivor", codec="vorbis", "survivor.ogg")
  test.pass()
end

thread.run(delay=0.1, run)
