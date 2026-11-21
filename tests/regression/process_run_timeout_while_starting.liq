%include "../test.liq"

# `process.run` honours its timeout as soon as the scheduler runs tasks: the
# start hooks that follow may still be running.
def check() =
  p =
    process.run(
      timeout=0.5,
      "sleep 5"
    )
  if p.status == "timeout" then test.pass() else test.fail() end
end

on_start(
  fun () ->
    begin
      thread.run(delay=0.1, check)
      thread.pause(2.)
    end
)

output.dummy(blank())
