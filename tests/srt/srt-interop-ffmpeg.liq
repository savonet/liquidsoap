# Every role and kind against a third-party SRT peer, the FFmpeg command line
# tool (spec/conformance.md §1).
%include "../test.liq"
%include "helpers.inc"

def ffmpeg(args) =
  r =
    process.run(
      timeout=30.,
      "ffmpeg -hide_banner -loglevel error -nostdin #{args}"
    )
  if
    r.status != "exit" or r.status.code != 0
  then
    test.fail(
      "ffmpeg #{args}: #{r.status} #{r.stderr}"
    )
  end
end

if
  not string.contains(
    substring=" srt\n",
    process.read(
      "ffmpeg -hide_banner -protocols"
    )
  )
then
  print(
    "ffmpeg without SRT support: interoperability not covered"
  )
  test.skip()
end

send_ts =
  "-re -f lavfi -i sine=duration=4 -c:a aac -f mpegts"

listener_input = input.srt(port=19542)
output.dummy(fallible=true, listener_input)
caller_input =
  input.srt(mode="caller", host="127.0.0.1", port=19544, polling_delay=0.2)
output.dummy(fallible=true, caller_input)
ts = %ffmpeg(format = "mpegts", %audio(codec = "aac"))
output.srt(mode="listener", port=19546, ts, sine())
output.srt(host="127.0.0.1", port=19548, polling_delay=0.2, ts, sine())

def run() =
  thread.run(
    fun () ->
      ffmpeg(
        "#{send_ts} 'srt://127.0.0.1:19542?mode=caller'"
      )
  )
  wait_until(
    what="listener input decoding ffmpeg",
    {listener_input.is_ready()}
  )
  thread.run(
    fun () ->
      ffmpeg(
        "#{send_ts} 'srt://0.0.0.0:19544?mode=listener'"
      )
  )
  wait_until(
    what="caller input decoding ffmpeg",
    {caller_input.is_ready()}
  )
  ffmpeg(
    "-i 'srt://127.0.0.1:19546?mode=caller' -t 3 -c copy -y from-listener.ts"
  )
  check_decodes(
    what="ffmpeg from listener output",
    codec="aac",
    "from-listener.ts"
  )
  ffmpeg(
    "-i 'srt://0.0.0.0:19548?mode=listener' -t 3 -c copy -y from-caller.ts"
  )
  check_decodes(
    what="ffmpeg from caller output",
    codec="aac",
    "from-caller.ts"
  )
  test.pass()
end

thread.run(delay=0.1, run)
