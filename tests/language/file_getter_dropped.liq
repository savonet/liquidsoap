#!../../liquidsoap

%include "../test.liq"

# The watch behind `file.getter` lives as long as any copy of the getter: the
# getter returned by a function is a copy.
dir = file.temp_dir(cleanup=true, "file_getter_dropped")
watched = path.concat(dir, "watched")

def make_getter() =
  file.write(data="before", watched)
  file.getter(watched)
end

def f() =
  getter = make_getter()
  test.equal(getter(), "before")
  deadline = time() + 20.
  rounds = ref(0)
  changed = ref(false)

  def check() =
    runtime.gc.full_major()
    ref.incr(rounds)
    if
      rounds() >= 3 and not changed()
    then
      file.write(data="after", watched)
      changed := true
      0.5
    elsif
      changed() and getter() == "after"
    then
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "Getter still reads #{getter()} after #{rounds()} rounds"
      )
      (-1.)
    else
      0.5
    end
  end

  thread.run.recurrent(delay=0., check)
end

test.check(f)
