%include "../test.liq"

# Poll `cond` until it holds, or fail the test naming `what`.
def wait_for(~timeout=30., what, cond) =
  deadline = time() + timeout
  def rec loop() =
    if
      not cond()
    then
      if
        time() > deadline
      then
        test.fail(
          "timeout waiting for #{what}"
        )
      end
      thread.pause(0.02)
      loop()
    end
  end
  loop()
end

# files.md §5: with segments_overhead null every segment stays on disk, and
# the saved state stays bounded by the window.
dir = file.temp_dir("hls_overhead_null_state")
on_cleanup({file.rmdir(dir)})

o =
  output.file.hls(
    segment_duration=1.,
    segments=3,
    segments_overhead=null,
    persist_at="state.json",
    dir,
    [("a", %mp3)],
    sine()
  )

clock.assign_new(sync="none", [o])

def check() =
  wait_for(
    "40 segments",
    {file.exists("#{dir}/a_40.mp3")}
  )
  o.stop()
  kept = list.filter(fun (f) -> r/^a_\d+\.mp3$/.test(f), file.ls(dir))
  if
    list.length(kept) < 40
  then
    test.fail(
      "only #{list.length(kept)} segments kept on disk"
    )
  end
  state = file.contents("#{dir}/state.json")
  saved = list.length(string.split(separator='"name"', state)) - 1
  if
    saved < 3 or saved > 20
  then
    test.fail(
      "#{saved} names in the saved state, expected the window's few"
    )
  end
  test.pass()
end

thread.run(check)
