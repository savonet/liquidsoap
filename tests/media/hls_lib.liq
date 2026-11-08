# Readers for the HLS output, shared by the hls_* tests: `hls_test.playlist`
# parses a media playlist, `hls_test.packets` probes segments with ffprobe,
# `hls_test.starts` gives each segment's start from its EXTINFs and
# `hls_test.check` counts and reports checks.
let hls_test = ()

let hls_test.checks = ref(0)

def hls_test.check(ok, msg) =
  ref.incr(hls_test.checks)
  if not ok then test.fail(msg) end
end

# Fail unless at least `n` checks ran, then pass.
def hls_test.done(n) =
  if
    hls_test.checks() < n
  then
    test.fail(
      "only #{hls_test.checks()} checks ran, expected at least #{n}"
    )
  end
  print(
    "#{hls_test.checks()} checks passed"
  )
  test.pass()
end

def hls_test.header(lines, tag) =
  list.fold(
    fun (cur, l) ->
      if
        string.contains(prefix="#{tag}:", l)
      then
        string.residual(prefix="#{tag}:", l)
      else
        cur
      end,
    (null : string?),
    lines
  )
end

def hls_test.playlist(playlist_path) =
  lines = r/\r\n/.split(file.contents(playlist_path))
  lines = list.filter(fun (l) -> l != "", lines)
  segments = ref([])
  discontinuous = ref(false)
  map = ref("")
  tags = ref([])
  extinf = ref(0.)
  list.iter(
    fun (l) ->
      if
        l == "#EXT-X-DISCONTINUITY"
      then
        discontinuous := true
      elsif
        string.contains(prefix="#EXT-X-MAP:URI=\"", l)
      then
        m = null.get(string.residual(prefix="#EXT-X-MAP:URI=\"", l))
        map := string.sub(m, start=0, length=string.length(m) - 1)
      elsif
        string.contains(prefix="#EXTINF:", l)
      then
        d = null.get(string.residual(prefix="#EXTINF:", l))
        extinf :=
          float_of_string(string.sub(d, start=0, length=string.length(d) - 1))
      elsif
        string.contains(prefix="#", l)
      then
        tags := [...tags(), l]
      else
        segments :=
          [
            ...segments(),
            {
              uri = l,
              duration = extinf(),
              discontinuous = discontinuous(),
              map = map(),
              tags = tags()
            }
          ]
        discontinuous := false
        tags := []
      end,
    lines
  )

  def int_header(tag) =
    v = hls_test.header(lines, tag)
    null.defined(v) ? int_of_string(null.get(v)) : (-1)
  end

  {
    lines = lines,
    target = int_header("#EXT-X-TARGETDURATION"),
    media_sequence = int_header("#EXT-X-MEDIA-SEQUENCE"),
    discontinuity_sequence = int_header("#EXT-X-DISCONTINUITY-SEQUENCE"),
    independent = list.mem("#EXT-X-INDEPENDENT-SEGMENTS", lines),
    segments = segments()
  }
end

# Each segment's start in media time: the sum of the EXTINFs before it.
def hls_test.starts(segments) =
  let (_, starts) =
    list.fold(
      fun (acc, s) ->
        begin
          let (t, l) = acc
          (t + s.duration, [...l, t])
        end,
      (0., []),
      segments
    )
  starts
end

# The packets of a segment, in file order. With `init`, the init segment is
# prepended so that an fMP4 media segment can be probed.
def hls_test.packets(~init:init_file="", segment) =
  probed =
    if
      init_file == ""
    then
      segment
    else
      f = file.temp("hls-probe", ".mp4")
      file.write(data=file.contents(init_file) ^ file.contents(segment), f)
      f
    end

  probe =
    process.read(
      "ffprobe -v error -show_packets -show_entries \
       packet=codec_type,stream_index,pts_time,dts_time,flags -of json #{
        process.quote(probed)
      }"
    )

  if init_file != "" then file.remove(probed) end

  let json.parse ({packets} :
    {
      packets: [
        {
          codec_type: string,
          stream_index: int,
          pts_time?: string,
          dts_time?: string,
          flags: string
        }
      ]
    }
  ) = probe

  list.map(
    fun (p) ->
      {
        type = p.codec_type,
        stream = p.stream_index,
        pts = float_of_string(p?.pts_time ?? "nan"),
        dts = float_of_string(p?.dts_time ?? "nan"),
        key = string.contains(prefix="K", p.flags)
      },
    packets
  )
end

# The first packet of each audio and video stream of a segment.
def hls_test.first_packets(packets) =
  let (_, firsts) =
    list.fold(
      fun (acc, p) ->
        begin
          let (seen, firsts) = acc
          if
            (p.type == "audio" or p.type == "video")
            and not list.mem(p.stream, seen)
          then
            ([...seen, p.stream], [...firsts, p])
          else
            acc
          end
        end,
      ([], []),
      packets
    )
  firsts
end

# The earliest pts of the audio and video packets of a segment.
def hls_test.first_pts(packets) =
  list.fold(
    fun (m, p) ->
      if
        (p.type == "audio" or p.type == "video") and p.pts < m
      then
        p.pts
      else
        m
      end,
    infinity,
    packets
  )
end

# Whether a MPEG-TS file starts with a PAT then a PMT, after the SDT that
# FFmpeg's muxer writes with them.
def hls_test.starts_with_pat_pmt(segment) =
  data = file.contents(segment)
  packets = string.length(data) / 188
  def pid(n) =
    off = n * 188
    (string.nth(data, off + 1) mod 32) * 256 + string.nth(data, off + 2)
  end
  def table_id(n) =
    off = n * 188
    string.nth(data, off + 5 + string.nth(data, off + 4))
  end
  first = packets > 0 and pid(0) == 17 ? 1 : 0
  packets >= first + 2
  and string.nth(data, first * 188) == 71
  and pid(first) == 0
  and table_id(first) == 0
  and table_id(first + 1) == 2
end

# Lines of the log file containing `s`.
def hls_test.log_lines(log_path, s) =
  log.flush()
  list.filter(
    fun (l) -> string.contains(substring=s, l),
    r/\n/.split(file.contents(log_path))
  )
end

# Run `f` in a thread at the first stop of `o`, so that the final stop that
# `test.pass` causes does not run it again.
def hls_test.at_first_stop(o, f) =
  stopped = ref(false)
  o.on_stop(
    synchronous=false,
    fun () ->
      if
        not stopped()
      then
        stopped := true
        thread.run(f)
      end
  )
end
