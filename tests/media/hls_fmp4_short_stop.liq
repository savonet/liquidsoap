%include "../test.liq"
%include "hls_lib.liq"

# encoder-contract.md §8: an audio-only fMP4 variant stopped within its first
# segment learns its init segment at its stop, and its playlist names it.
dir = file.temp_dir(cleanup=true, "hls-fmp4-short-stop")
s = once(sine(duration=1.5))
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    fallible=true,
    segment_duration=10.,
    dir,
    [
      (
        "a",
        %ffmpeg(
          format = "mp4",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

def check() =
  p = hls_test.playlist("#{dir}/a.m3u8")
  hls_test.check(
    list.length(p.segments) == 1,
    "expected one segment, got #{list.length(p.segments)}"
  )
  list.iter(
    fun (seg) ->
      begin
        hls_test.check(
          seg.map != "" and file.exists("#{dir}/#{seg.map}"),
          "#{seg.uri} has EXT-X-MAP #{string.quote(seg.map)}"
        )
        hls_test.check(
          list.exists(
            fun (pk) -> pk.type == "audio",
            hls_test.packets(init="#{dir}/#{seg.map}", "#{dir}/#{seg.uri}")
          ),
          "#{seg.uri} holds no audio packet with its init segment"
        )
      end,
    p.segments
  )
  hls_test.done(3)
end

hls_test.at_first_stop(o, check)
