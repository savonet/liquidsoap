%include "../test.liq"

# files.md §5, known-complexity.md KC7: with `segments=3` and
# `segments_overhead=1`, a segment that left the window stays on disk while
# either allowance holds, and is gone one close after neither holds.
dir = file.temp_dir("hls_retention")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
segments_wanted = 30

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def window_of(playlist_path) =
  lines =
    list.filter(
      fun (l) -> l != "",
      string.split(separator="\r\n", file.contents(playlist_path))
    )

  def parse(acc, l) =
    let (length, window) = acc
    if
      string.contains(prefix="#EXTINF:", l)
    then
      m = r/^#EXTINF:([0-9.]+),$/.exec(l)
      (float_of_string(m[1]), window)
    elsif
      string.contains(prefix="#", l)
    then
      acc
    else
      (0., [...window, (l, length)])
    end
  end

  snd(list.fold(parse, (0., []), lines))
end

# Each segment seen: its length, the longest window that listed it, and,
# once it left, the media time closed then and its rank among leavers.
seen = ref([])
closed_total = ref(0.)
left_count = ref(0)
present_checks = ref(0)
absent_checks = ref(0)

def update_seen(window) =
  window_duration = list.fold(fun (t, el) -> t + snd(el), 0., window)
  list.iter(
    fun (el) ->
      begin
        let (uri, length) = el
        if
          not list.assoc.mem(uri, seen())
        then
          closed_total := closed_total() + length
          seen :=
            [
              ...seen(),
              (
                uri,
                {
                  length = length,
                  longest = 0.,
                  left_at = -1.,
                  rank = -1,
                  needed = true,
                  needed_before = true
                }
              )
            ]
        end
      end,
    window
  )

  def update(el) =
    let (uri, segment) = el
    listed = list.assoc.mem(uri, window)
    leaving = not listed and segment.left_at < 0.
    if leaving then left_count := left_count() + 1 end
    left_at = leaving ? closed_total() : (segment.left_at)
    rank = leaving ? left_count() - 1 : (segment.rank)
    longest = listed ? max(segment.longest, window_duration) : (segment.longest)
    available = closed_total() - left_at < segment.length + longest - 0.0005
    overhead = left_count() - 1 - rank < 1
    (
      uri,
      {
        length = segment.length,
        longest = longest,
        left_at = left_at,
        rank = rank,
        needed = left_at < 0. or available or overhead,
        needed_before = segment.needed
      }
    )
  end

  seen := list.map(update, seen())
end

def check_disk() =
  list.iter(
    fun (el) ->
      begin
        let (uri, segment) = el
        on_disk = file.exists("#{out}/#{uri}")
        if
          segment.left_at >= 0.
        then
          if
            segment.needed
          then
            if
              not on_disk
            then
              test.fail(
                "#{uri} deleted while retention holds: #{
                  closed_total() - segment.left_at
                }s closed since it left, length #{segment.length}, longest \
                 window #{segment.longest}, #{left_count() - 1 - segment.rank} \
                 left after it"
              )
            end
            present_checks := present_checks() + 1
          elsif
            not segment.needed_before
          then
            if
              on_disk
            then
              test.fail(
                "#{uri} still on disk past both retention allowances: #{
                  closed_total() - segment.left_at
                }s closed since it left, length #{segment.length}, longest \
                 window #{segment.longest}"
              )
            end
            absent_checks := absent_checks() + 1
          end
        end
      end,
    seen()
  )
end

def on_change(event) =
  if
    not test.done()
  then
    if
      event.path == "#{out}/a.m3u8" and event.state != "deleted"
    then
      update_seen(window_of(event.path))
    elsif
      string.contains(prefix="#{out}/a_", event.path)
      and event.state == "created"
    then
      check_disk()
      if
        event.path == "#{out}/a_#{segments_wanted}.mp3"
      then
        if
          present_checks() == 0 or absent_checks() == 0
        then
          test.fail(
            "too few checks: #{present_checks()} kept, #{absent_checks()} \
             deleted"
          )
        end
        print(
          "#{present_checks()} kept and #{absent_checks()} deleted segments \
           checked"
        )
        test.pass()
      end
    end
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segments=3,
    segments_overhead=1,
    out,
    [("a", %mp3)],
    s
  )

o.on_file_change(synchronous=true, on_change)
