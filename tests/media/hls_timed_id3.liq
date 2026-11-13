%include "../test.liq"
%include "hls_lib.liq"

# KC18: titles changing off the grid in an AAC MPEG-TS variant with timed ID3,
# beside the same variant with `id3=false`.
dir = file.temp_dir(cleanup=true, "hls-id3")

changes = [(0., "alpha"), (5.3, "bravo"), (9.7, "charlie")]

def track(n) =
  let (start, title) = list.nth(changes, n)
  stop =
    n + 1 < list.length(changes) ? fst(list.nth(changes, n + 1)) : (start + 4.)
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    once(sine(duration=stop - start))
  )
end

s = sequence([track(0), track(1), track(2)])
clock.assign_new(sync="none", [s])

# Tracks end on internal frames: the media times of the changes are read here.
change_times = ref([])
s.on_metadata(
  synchronous=true,
  fun (m) -> change_times := [...change_times(), (s.time(), m["title"])]
)

aac =
  %ffmpeg(
    format = "mpegts",
    %audio(codec = "aac", samplerate = 44100, channels = 2)
  )

o =
  output.file.hls(
    fallible=true,
    segment_duration=2.,
    segments=100,
    segments_overhead=null,
    dir,
    [("id3", aac), ("noid3", aac.{id3 = false})],
    s
  )

aac_frame = 1024. / 44100.

# The UTF-16LE bytes of an ASCII string, as hex: ID3v2.3 writes titles so.
def hex(str) =
  string.concat(
    list.map(
      fun (c) ->
        begin
          h = string.hex_of_int(c)
          string.length(h) == 1 ? "0#{h}00" : "#{h}00"
        end,
      list.map(
        fun (i) -> string.nth(str, i),
        list.init(string.length(str), fun (i) -> i)
      )
    )
  )
end

# The timed ID3 packets of a segment, with the bytes of each as hex, read from
# ffprobe's hex dump.
def id3_packets(segment) =
  probe =
    process.read(
      "ffprobe -v error -select_streams d -show_packets -show_data -show_entries \
       packet=pts_time,data -of json #{process.quote(segment)}"
    )
  let json.parse ({packets} : {packets: [{pts_time: string, data: string}]}) =
    probe
  list.map(
    fun (p) ->
      begin
        lines = r/\n/.split(p.data)
        bytes =
          list.map(
            fun (l) ->
              if
                string.length(l) > 49
              then
                string.replace(
                  pattern=" ",
                  fun (_) -> "",
                  string.sub(l, start=10, length=39)
                )
              else
                ""
              end,
            lines
          )
        {pts = float_of_string(p.pts_time), hex = string.concat(bytes)}
      end,
    packets
  )
end

def title_of(packet) =
  list.fold(
    fun (cur, c) ->
      begin
        let (_, title) = c
        string.contains(substring=hex(title), packet.hex) ? title : (cur)
      end,
    "",
    change_times()
  )
end

def current_title(t) =
  list.fold(
    fun (cur, c) -> fst(c) <= t + aac_frame ? snd(c) : (cur),
    "",
    change_times()
  )
end

def check() =
  p = hls_test.playlist("#{dir}/id3.m3u8")
  n = list.length(p.segments)
  hls_test.check(
    n >= 6,
    "id3: only #{n} segments"
  )
  starts = hls_test.starts(p.segments)
  per_segment =
    list.map(fun (seg) -> id3_packets("#{dir}/#{seg.uri}"), p.segments)
  all = list.flatten(per_segment)
  hls_test.check(
    all != [],
    "id3: no timed ID3 packet"
  )
  origin = list.hd(all).pts
  hls_test.check(
    title_of(list.hd(all)) == "alpha",
    "id3: the first ID3 packet carries #{title_of(list.hd(all))}, not alpha"
  )

  ignore(
    list.fold(
      fun (last, pk) ->
        begin
          hls_test.check(
            pk.pts > last,
            "id3: tag time #{pk.pts} after #{last}, times must increase"
          )
          pk.pts
        end,
      0. - infinity,
      all
    )
  )

  list.iter(
    fun (c) ->
      begin
        let (t, title) = c
        tagged =
          list.exists(
            fun (pk) ->
              title_of(pk) == title and abs(pk.pts - origin - t) <= aac_frame,
            all
          )
        hls_test.check(
          tagged,
          "id3: no ID3 packet with #{title} at #{t}s: #{
            list.map(fun (pk) -> (pk.pts - origin, title_of(pk)), all)
          }"
        )
      end,
    change_times()
  )
  hls_test.check(
    list.length(change_times()) == 3,
    "metadata changes seen: #{change_times()}"
  )

  list.iteri(
    fun (i, packets) ->
      begin
        start = list.nth(starts, i)
        expected = current_title(start)
        first =
          packets == []
            ? "none"
            : "#{title_of(list.hd(packets))} at #{
              list.hd(packets).pts - origin
            }"
        hls_test.check(
          packets != []
          and abs(list.hd(packets).pts - origin - start) <= aac_frame
          and title_of(list.hd(packets)) == expected,
          "id3: segment #{i} starts at #{start} with ID3 #{first}, expected #{
            expected
          } at its start"
        )
      end,
    per_segment
  )

  q = hls_test.playlist("#{dir}/noid3.m3u8")
  hls_test.check(
    list.length(q.segments) >= 6,
    "noid3: too few segments"
  )
  list.iter(
    fun (seg) ->
      hls_test.check(
        id3_packets("#{dir}/#{seg.uri}") == [],
        "noid3: #{seg.uri} carries ID3 packets"
      ),
    q.segments
  )
  hls_test.done(25)
end

hls_test.at_first_stop(o, check)
