%include "../test.liq"
%include "hls_lib.liq"

# Every segment decodes on its own: each of its video packets gives a frame
# when ffprobe starts at the segment. The key-frame interval is the HLS
# default, one key frame per 1 s segment, closed GOP.
settings.frame.video.width := 320
settings.frame.video.height := 240

root = file.temp_dir(cleanup=true, "hls-independent")
s = source({video = source.tracks(video.testsrc.ffmpeg()).video})
clock.assign_new(sync="none", [s])
stopped = ref(0)

# MPEG-TS lists the stream twice, under its program and alone: the first line
# is enough.
def decoded(segment) =
  output =
    process.read(
      "ffprobe -v quiet -select_streams v -count_frames -count_packets \
       -show_entries stream=nb_read_frames,nb_read_packets -of csv=p=0 #{
        process.quote(segment)
      }"
    )
  fields =
    string.split(
      separator=",",
      list.hd(default="", string.split(separator="\n", output))
    )
  def field(n) =
    int_of_string(default=-1, string.trim(list.nth(default="", fields, n)))
  end
  (field(0), field(1))
end

def check(name, dir) =
  p = hls_test.playlist("#{dir}/#{name}.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 4,
    "#{name}: only #{n} segments listed"
  )
  list.iter(
    fun (seg) ->
      begin
        let (frames, packets) = decoded("#{dir}/#{seg.uri}")
        hls_test.check(
          packets > 0 and frames == packets,
          "#{name}: #{seg.uri} decodes #{frames} of #{packets} frames"
        )
      end,
    p.segments
  )
end

def make(name, codec) =
  dir = "#{root}/#{name}"
  o =
    output.file.hls(
      fallible=true,
      segment_duration=1.,
      segments=100,
      segments_overhead=null,
      dir,
      [(name, %ffmpeg(format = "mpegts", %video(codec = codec)))],
      s
    )
  o.on_file_change(
    synchronous=false,
    fun (ev) ->
      if
        ev.state == "created" and path.basename(ev.path) == "#{name}_6.ts"
      then
        o.stop()
      end
  )
  hls_test.at_first_stop(
    o,
    fun () ->
      begin
        check(name, dir)
        ref.incr(stopped)
        if stopped() == 2 then hls_test.done(10) end
      end
  )
end

make("x264", "libx264")
make("x265", "libx265")
