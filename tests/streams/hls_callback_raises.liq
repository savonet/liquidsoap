%include "../test.liq"

# files.md §4, operator.md §6: a synchronous callback raising on a segment's
# `created` event ends the run, the segment is still listed, and numbering
# has no gap after the new run starts.
dir = file.temp_dir("hls_callback_raises")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
variants = ["a", "b"]

s = sine()
clock.assign_new(sync="none", [s])

raised = ref(false)
reopen_count = ref(0)

def reopen_after(_) =
  reopen_count := reopen_count() + 1
  0.1
end

def raise_once(event) =
  if
    not raised() and event.path == "#{out}/a_2.mp3"
  then
    raised := true
    error.raise(
      error.failure,
      "callback failure on purpose"
    )
  end
end

def positions_of(v) =
  lines = string.split(separator="\r\n", file.contents("#{out}/#{v}.m3u8"))
  list.filter_map(
    fun (l) ->
      begin
        m = r/_([0-9]+)\.mp3$/.exec(l)
        m == [] ? null : int_of_string(m[1])
      end,
    lines
  )
end

listed = ref([])

def check(event) =
  if
    not test.done()
  then
    list.iter(
      fun (v) ->
        if
          event.path == "#{out}/#{v}.m3u8" and event.state != "deleted"
        then
          positions = positions_of(v)
          list.iteri(
            fun (n, p) ->
              if
                p != list.hd(positions) + n
              then
                test.fail(
                  "variant #{v} lists positions #{positions}, with a gap"
                )
              end,
            positions
          )
          list.iter(
            fun (p) ->
              if
                not list.mem((v, p), listed())
              then
                listed := (v, p)::listed()
              end,
            positions
          )
        end,
      variants
    )
    if
      event.path == "#{out}/b_8.mp3"
    then
      if
        reopen_count() != 1
      then
        test.fail(
          "reopen_on_error called #{reopen_count()} times, expected once"
        )
      end
      if
        not list.mem(("a", 2), listed())
      then
        test.fail(
          "a_2.mp3 never listed"
        )
      end
      list.iter(
        fun (v) ->
          list.iter(
            fun (p) ->
              if
                not list.mem((v, p), listed())
              then
                test.fail(
                  "#{v}_#{p} never listed: numbering has a gap"
                )
              end,
            [1, 2, 3, 4, 5, 6, 7]
          ),
        variants
      )
      test.pass()
    end
  end
end

o =
  output.file.hls(
    reopen_on_error=reopen_after,
    segment_duration=1.,
    out,
    [("a", %mp3), ("b", %shine(bitrate = 64))],
    s
  )

o.on_file_change(synchronous=true, raise_once)
o.on_file_change(synchronous=true, check)
