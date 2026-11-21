%include "../test.liq"

# A playlist that does not loop calls `on_done` once, when it runs out.
done = ref(0)
p = playlist.list(loop=false, on_done={ref.incr(done)}, ["file1.mp3"])
output.dummy(fallible=true, p)

def check() =
  deadline = time() + 20.

  def poll() =
    if
      done() > 0
    then
      thread.run(
        delay=2.,
        {
          test.equal(done(), 1)
          test.pass()
        }
      )
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "on_done was never called"
      )
      (-1.)
    else
      0.2
    end
  end

  thread.run.recurrent(delay=0., poll)
end

test.check(check)
