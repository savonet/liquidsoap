# A listener client joining a running output receives a stream it can decode:
# Ogg and WAV through their header, MPEG-TS as is, and Ogg right after a
# metadata change through the new link (spec/output.md §4).
%include "../test.liq"
%include "helpers.inc"

s = sine()
o_ogg = output.srt(mode="listener", port=19502, %ogg(%vorbis), s)
o_wav = output.srt(mode="listener", port=19504, %wav, sine())
o_ts =
  output.srt(
    mode="listener",
    port=19506,
    %ffmpeg(format = "mpegts", %audio(codec = "aac")),
    sine()
  )

pending = ref(0)

def client(~port, ~file_name, ~duration) =
  atomic({pending := pending() + 1})
  thread.run(
    fun () ->
      begin
        code =
          run_peer(
            "receive --port #{port} --bytes #{file_name} --duration #{duration}"
          )
        if
          code != 0
        then
          test.fail(
            "#{file_name}: no connection"
          )
        end
        atomic({pending := pending() - 1})
      end
  )
end

def joins(~what, o, ~port, ~prefix) =
  client(port=port, file_name="#{prefix}-first.bin", duration=16.)
  wait_until(
    what="#{what} first client",
    {list.length(o.sockets()) == 1}
  )
  thread.pause(1.5)
  client(port=port, file_name="#{prefix}-late.bin", duration=5.)
end

def run() =
  joins(what="ogg", o_ogg, port=19502, prefix="ogg")
  joins(what="wav", o_wav, port=19504, prefix="wav")
  joins(what="ts", o_ts, port=19506, prefix="ts")
  wait_until(
    what="two ogg clients",
    {list.length(o_ogg.sockets()) == 2}
  )
  thread.pause(0.5)
  s.insert_metadata(
    [
      (
        "title",
        "new link"
      )
    ]
  )
  client(port=19502, file_name="ogg-metadata.bin", duration=5.)
  wait_until(
    timeout=30.,
    what="every client",
    {pending() == 0}
  )
  check_decodes(
    what="ogg late client",
    codec="vorbis",
    "ogg-late.bin"
  )
  check_decodes(
    what="ogg client after metadata",
    codec="vorbis",
    "ogg-metadata.bin"
  )
  check_decodes(
    what="wav late client",
    codec="pcm_s16le",
    "wav-late.bin"
  )
  check_decodes(
    what="ts late client",
    codec="aac",
    "ts-late.bin"
  )
  test.pass()
end

thread.run(delay=0.1, run)
