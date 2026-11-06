%include "../test.liq"

# Poll `cond` until it holds, or fail the test naming `what`.
def wait_for(~timeout=20., what, cond) =
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

# persistence.md §3: an output that sleeps without having started leaves the
# previous run's state file as it is.
dir = file.temp_dir("hls_persist_unstarted_sleep")
on_cleanup({file.rmdir(dir)})
previous =
  "the previous run's state"
file.write(data=previous, "#{dir}/state.json")

o =
  output.file.hls(
    start=false,
    persist_at="state.json",
    segment_duration=1.,
    dir,
    [("a", %mp3)],
    sine()
  )

clock.assign_new(sync="none", [o])

awake = ref(false)
o.on_wake_up(synchronous=true, {awake := true})
o.on_shutdown(synchronous=true, {awake := false})

def check() =
  wait_for(
    "the output to wake up",
    {awake()}
  )
  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
  content = file.contents("#{dir}/state.json")
  if
    content != previous
  then
    test.fail(
      "the state file was replaced at sleep: #{content}"
    )
  end
  test.pass()
end

thread.run(check)
