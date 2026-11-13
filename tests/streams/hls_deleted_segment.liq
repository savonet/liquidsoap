%include "../test.liq"

# files.md §5.1, known-complexity.md KC15: a retained segment deleted by hand
# is logged as a warning naming its path when retention deletes it, and
# streaming continues.
dir = file.temp_dir("hls_deleted_segment")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
log_path = "#{dir}/log"
log.file := true
log.file.path := log_path
removed = "#{out}/a_4.mp3"

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def check_log() =
  deadline = time() + 20.

  def rec wait() =
    lines = file.exists(log_path) ? file.lines(log_path) : []
    warned =
      list.exists(
        fun (l) ->
          string.contains(substring=removed, l)
          and (
            string.contains(
              substring=":2] ",
              l
            )
            or string.contains(
              substring=":3] ",
              l
            )
          ),
        lines
      )

    if
      warned
    then
      print(
        "deletion of #{removed} logged, streaming continued"
      )
      test.pass()
    elsif
      time() > deadline
    then
      test.fail(
        "no warning naming #{removed} in #{log_path}"
      )
    else
      thread.pause(0.1)
      wait()
    end
  end

  wait()
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

o.on_file_change(
  synchronous=true,
  fun (event) ->
    if
      event.state == "created"
    then
      if
        event.path == "#{out}/a_8.mp3"
      then
        if
          not file.exists(removed)
        then
          test.fail(
            "#{removed} is already gone at a_8"
          )
        end
        file.remove(removed)
      end
      if event.path == "#{out}/a_16.mp3" then thread.run(check_log) end
    end
)
