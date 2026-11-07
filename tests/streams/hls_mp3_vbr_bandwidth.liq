%include "../test.liq"

# variants.md §3, known-complexity.md KC9: an `%mp3` VBR variant without
# `bandwidth` announces the measured peak segment bit rate of its listed
# segments, with some headroom.
dir = file.temp_dir("hls_mp3_vbr_bandwidth")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
segments_wanted = 12

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def lines_of(playlist_path) =
  list.filter(
    fun (l) -> l != "",
    string.split(separator="\r\n", file.contents(playlist_path))
  )
end

# Listed segments of the media playlist, as (bytes, seconds).
def listed_segments() =
  def parse(acc, l) =
    let (length, segments) = acc
    if
      string.contains(prefix="#EXTINF:", l)
    then
      m = r/^#EXTINF:([0-9.]+),$/.exec(l)
      (float_of_string(m[1]), segments)
    elsif
      string.contains(prefix="#", l)
    then
      acc
    else
      (0., [...segments, (file.size("#{out}/#{l}"), length)])
    end
  end

  snd(list.fold(parse, (0., []), lines_of("#{out}/a.m3u8")))
end

# RFC 8216 §4.1 peak segment bit rate over runs lasting 0.5 to 1.5 T, T = 1.
def peak_bitrate(segments) =
  def rec runs(best, segments) =
    def rec extend(best, bytes, length, rest) =
      if
        rest == [] or length > 1.5
      then
        best
      else
        let [(b, l), ...rest] = rest
        bytes = bytes + b
        length = length + l
        rate = float(bytes * 8) / length
        best = length >= 0.5 and length <= 1.5 and rate > best ? rate : (best)
        extend(best, bytes, length, rest)
      end
    end

    if
      segments == []
    then
      best
    else
      runs(extend(best, 0, 0., segments), list.tl(segments))
    end
  end

  if
    list.length(segments) == 1
  then
    let [(b, l)] = segments
    float(b * 8) / l
  else
    runs(0., segments)
  end
end

checks = ref(0)

def check_main(event) =
  if
    not test.done()
    and event.path == "#{out}/stream.m3u8"
    and event.state != "deleted"
  then
    line =
      list.find(
        default="",
        fun (l) -> string.contains(prefix="#EXT-X-STREAM-INF:", l),
        lines_of(event.path)
      )

    m = r/BANDWIDTH=([0-9]+)/.exec(line)
    bandwidth = float(int_of_string(default=0, m[1]))
    measured = peak_bitrate(listed_segments())
    if
      measured <= 0.
      or bandwidth < measured * 0.995
      or bandwidth > measured * 1.25
    then
      test.fail(
        "announced BANDWIDTH #{bandwidth} for a measured peak of #{measured}: #{
          line
        }"
      )
    end
    checks := checks() + 1
  end
  if
    not test.done() and event.path == "#{out}/a_#{segments_wanted}.mp3"
  then
    if
      checks() < 2
    then
      test.fail(
        "only #{checks()} main playlists checked"
      )
    end
    print(
      "#{checks()} main playlists checked"
    )
    test.pass()
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    out,
    [("a", %mp3.vbr(quality = 4))],
    s
  )

o.on_file_change(synchronous=true, check_main)
