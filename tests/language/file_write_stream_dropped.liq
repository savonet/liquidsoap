#!../../liquidsoap

%include "../test.liq"

# An atomic writer dropped before being closed removes its temporary file.
dir = file.temp_dir(cleanup=true, "file_write_stream_dropped")
target = path.concat(dir, "dropped")

def hidden_files() =
  list.filter(fun (f) -> string.sub(f, start=0, length=1) == ".", file.ls(dir))
end

def drop_writer() =
  write = file.write.stream(atomic=true, target)
  write(
    "never published"
  )
end

def f() =
  drop_writer()
  test.equal(list.length(hidden_files()), 1)
  deadline = time() + 20.
  rounds = ref(0)

  def check() =
    runtime.gc.full_major()
    ref.incr(rounds)
    if
      hidden_files() == []
    then
      test.equal(file.exists(target), false)
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "#{hidden_files()} still there after #{rounds()} rounds"
      )
      (-1.)
    else
      0.5
    end
  end

  thread.run.recurrent(delay=0., check)
end

test.check(f)
