%include "../test.liq"

def f() =
  def rejected(name) =
    try
      ignore(on_signal(name, fun () -> ()))
      false
    catch _ : [error.invalid] do
      true
    end
  end
  test.equal(rejected("term"), true)
  test.equal(rejected("int"), true)
  test.equal(rejected("nope"), true)

  def kill(signal) =
    ignore(
      process.run(
        "kill -#{signal} #{process.pid()}"
      )
    )
  end

  # usr1 also reopens liquidsoap's log: both must keep running.
  def on_usr1() =
    calls = ref([])
    late = ref(null)
    ignore(on_signal("usr2", fun () -> null.get(late()).release()))
    ignore(on_signal("usr2", fun () -> calls := [...calls(), 1]))
    released = on_signal("usr2", fun () -> calls := [...calls(), 0])
    released.release()
    released.release()

    # Released after the signal was received: already queued, must not run.
    late := on_signal("usr2", fun () -> calls := [...calls(), 3])
    ignore(
      on_signal(
        "usr2",
        fun () ->
          begin
            calls := [...calls(), 2]
            test.equal(calls(), [1, 2])
            test.pass()
          end
      )
    )
    kill("USR2")
  end

  ignore(on_signal("usr1", on_usr1))
  kill("USR1")
end

test.check(f)
