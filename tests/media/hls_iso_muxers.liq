%include "../test.liq"
%include "hls_lib.liq"

# variants.md §2: the "mov" and "ipod" muxers of the ISO family write fMP4
# segments with an init segment.
dir = file.temp_dir(cleanup=true, "hls-iso-muxers")

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    segment_duration=1.,
    dir,
    [
      (
        "mov",
        %ffmpeg(
          format = "mov",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      ),
      (
        "ipod",
        %ffmpeg(
          format = "ipod",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

def box_at(data, offset) =
  string.sub(encoding="ascii", data, start=offset + 4, length=4)
end

def check_variant(name) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  hls_test.check(
    p.segments != [],
    "#{name}: empty playlist"
  )
  list.iter(
    fun (seg) ->
      begin
        hls_test.check(
          seg.map != "" and r/\.mp4$/.test(seg.map),
          "#{name}: #{seg.uri} has EXT-X-MAP #{string.quote(seg.map)}"
        )
        init = file.contents("#{dir}/#{seg.map}")
        hls_test.check(
          box_at(init, 0) == "ftyp",
          "#{name}: #{seg.map} does not start with ftyp"
        )
        hls_test.check(
          r/\.m4s$/.test(seg.uri)
          and string.index(
            substring="moof",
            file.contents("#{dir}/#{seg.uri}")
          ) >=
            0,
          "#{name}: #{seg.uri} is not an fMP4 fragment"
        )
        hls_test.check(
          list.exists(
            fun (pk) -> pk.type == "audio",
            hls_test.packets(init="#{dir}/#{seg.map}", "#{dir}/#{seg.uri}")
          ),
          "#{name}: #{seg.uri} holds no audio packet"
        )
      end,
    p.segments
  )
end

def check() =
  check_variant("mov")
  check_variant("ipod")
  hls_test.done(10)
end

started = ref(false)
o.on_file_change(
  synchronous=true,
  fun (ev) ->
    if
      not started() and path.basename(ev.path) == "ipod_3.m4s"
    then
      started := true
      thread.run(
        fun () ->
          begin
            o.stop()
            check()
          end
      )
    end
)
