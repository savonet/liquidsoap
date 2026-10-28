%include "../test.liq"

# An output with no listener costs the same whatever its number of delivery
# groups: idle with 1 group, then restarted with 64, the process uses about
# the same CPU and context switches.
settings.harbor.output.writers := 1
o = output.harbor(port=18127, mount="idle", %wav, sine())

def ctxt_switches(task) =
  lines =
    string.split(
      separator="\n",
      file.contents("/proc/self/task/#{task}/status")
    )
  list.fold(
    fun (n, line) ->
      if
        string.contains(prefix="voluntary_ctxt_switches:", line)
        or string.contains(prefix="nonvoluntary_ctxt_switches:", line)
      then
        n +
          int_of_string(
            string.trim(list.nth(string.split(separator=":", line), 1))
          )
      else
        n
      end,
    0,
    lines
  )
end

# CPU ticks and context switches of the whole process.
def sample() =
  stat =
    string.split(
      separator=" ",
      file.contents("/proc/self/stat")
    )
  cpu = int_of_string(list.nth(stat, 13)) + int_of_string(list.nth(stat, 14))
  switches =
    list.fold(
      fun (total, task) -> total + ctxt_switches(task),
      0,
      file.ls("/proc/self/task")
    )
  (cpu, switches)
end

def idle_cost() =
  let (cpu0, switches0) = sample()
  thread.pause(6.)
  let (cpu1, switches1) = sample()
  (cpu1 - cpu0, switches1 - switches0)
end

def wait_for(cond) =
  def rec loop(n) =
    if
      not cond()
    then
      if
        n == 0
      then
        test.fail(
          "output did not restart"
        )
      end
      thread.pause(0.1)
      loop(n - 1)
    end
  end
  loop(100)
end

def check() =
  if not file.exists("/proc/self/task") then test.skip() end
  let (cpu1, switches1) = idle_cost()
  settings.harbor.output.writers := 64
  o.stop()
  wait_for(fun () -> not o.is_started())
  o.start()
  wait_for(o.is_started)
  thread.pause(2.)
  let (cpu64, switches64) = idle_cost()
  print(
    "idle over 6s: 1 group cpu=#{cpu1} switches=#{switches1}; 64 groups cpu=#{
      cpu64
    } switches=#{switches64}"
  )
  if
    switches1 == 0 or cpu1 == 0
  then
    test.fail(
      "nothing measured"
    )
  end
  if
    switches64 > switches1 * 3 / 2 + 300
  then
    test.fail(
      "context switches grow with groups"
    )
  end
  if
    cpu64 > cpu1 * 3 / 2 + 20
  then
    test.fail(
      "CPU grows with groups"
    )
  end
  test.pass()
end

o.on_start(
  synchronous=true,
  fun () ->
    if
      settings.harbor.output.writers() == 1
    then
      thread.run(delay=3., check)
    end
)
