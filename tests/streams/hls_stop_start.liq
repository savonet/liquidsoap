%include "../test.liq"

# operator.md §3.2, files.md §5 and known-complexity.md KC17, KC11: a fallible
# source off for 2 s, five times: every file is kept over the gap, the next
# segment of every variant is discontinuous, numbering continues, and the main
# playlist exists once every variant has a segment.
dir = file.temp_dir("hls_stop_start")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
variants = ["a", "b"]
cycles_wanted = 5

frames = ref(0)
source_on = ref(true)
src = switch([({source_on()}, sine().{track_sensitive = false})])
ticker = blank()
clock.assign_new(sync="none", [src, ticker])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end
ticker_output = output.dummy(ticker)
ticker_output.on_frame(
  synchronous=true,
  fun () ->
    begin
      frames := frames() + 1
      source_on := frames() mod 350 < 250
    end
)

def entries_of(playlist_path) =
  lines =
    list.filter(
      fun (l) -> l != "",
      string.split(separator="\r\n", file.contents(playlist_path))
    )

  def parse(acc, l) =
    let (discontinuous, entries) = acc
    if
      l == "#EXT-X-DISCONTINUITY"
    then
      (true, entries)
    elsif
      string.contains(prefix="#", l)
    then
      acc
    else
      m = r/_([0-9]+)\.mp3$/.exec(l)
      (false, [...entries, (int_of_string(m[1]), discontinuous)])
    end
  end

  snd(list.fold(parse, (false, []), lines))
end

stopped_files = ref([])
deleted_while_stopped = ref([])
stopped = ref(false)
restarts = ref(0)
restart_pending = ref([])
last_position = ref([])
discontinuity_checks = ref(0)
main_checks = ref(0)

def record_stop() =
  stopped := true
  stopped_files := file.ls(absolute=true, recursive=true, out)
end

def check_start() =
  if
    stopped()
  then
    stopped := false
    if
      deleted_while_stopped() != []
    then
      test.fail(
        "files deleted while the output was stopped: #{deleted_while_stopped()}"
      )
    end
    list.iter(
      fun (f) ->
        if
          not file.exists(f)
        then
          test.fail(
            "#{f} disappeared over a stop and start"
          )
        end,
      stopped_files()
    )
    restarts := restarts() + 1
    restart_pending := variants
  end
end

def check_new_segments(v) =
  list.iter(
    fun (entry) ->
      begin
        let (segment_position, discontinuous) = entry
        if
          segment_position > list.assoc(default=0, v, last_position())
        then
          previous = list.assoc(default=0, v, last_position())
          if
            segment_position != previous + 1
          then
            test.fail(
              "variant #{v}: segment #{segment_position} follows #{previous}, \
               numbering has a gap"
            )
          end
          pending = list.mem(v, restart_pending())
          if
            pending and not discontinuous
          then
            test.fail(
              "variant #{v}: segment #{segment_position}, first after restart #{
                restarts()
              }, is not discontinuous"
            )
          end
          if
            not pending and discontinuous
          then
            test.fail(
              "variant #{v}: segment #{segment_position} is discontinuous \
               without a restart"
            )
          end
          if pending then discontinuity_checks := discontinuity_checks() + 1 end
          restart_pending := list.filter(fun (w) -> w != v, restart_pending())
          last_position := list.assoc.remove(v, last_position())
          last_position := (v, segment_position)::last_position()
        end
      end,
    entries_of("#{out}/#{v}.m3u8")
  )
end

def on_change(event) =
  if
    not test.done()
  then
    if
      stopped() and event.state == "deleted"
    then
      deleted_while_stopped := event.path::deleted_while_stopped()
    end
    list.iter(
      fun (v) ->
        if
          event.path == "#{out}/#{v}.m3u8" and event.state != "deleted"
        then
          check_new_segments(v)
        end,
      variants
    )
    if
      restarts() > 0 and restart_pending() == []
    then
      if
        not file.exists("#{out}/stream.m3u8")
      then
        test.fail(
          "no main playlist after restart #{restarts()} although every variant \
           has a segment"
        )
      end
      main_checks := main_checks() + 1
      if
        restarts() == cycles_wanted
      then
        if
          discontinuity_checks() != cycles_wanted * list.length(variants)
        then
          test.fail(
            "#{discontinuity_checks()} discontinuities checked, expected #{
              cycles_wanted * list.length(variants)
            }"
          )
        end
        print(
          "#{restarts()} restarts, #{discontinuity_checks()} discontinuities, #{
            main_checks()
          } main playlists checked"
        )
        test.pass()
      end
    end
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    fallible=true,
    segment_duration=1.,
    segments=3,
    segments_overhead=1,
    out,
    [("a", %mp3), ("b", %shine(bitrate = 64))],
    src
  )

o.on_stop(synchronous=true, record_stop)
o.on_start(synchronous=true, check_start)
o.on_file_change(synchronous=true, on_change)
