#!../../liquidsoap

%include "../test.liq"

threads = 6
records_per_thread = 4

# Larger than one Unix.write chunk, so that an unlocked write can interleave.
record_length = 100000

def make_record(index) =
  string.make(char_code=65 + index, record_length)
end

def temp_files(dir) =
  list.filter(fun (f) -> string.sub(f, start=0, length=1) == ".", file.ls(dir))
end

def check_concurrent_writes(dir, target) =
  content = file.contents(target)
  lines = list.filter(fun (l) -> l != "", string.split(separator="\n", content))
  test.equal(list.length(lines), threads * records_per_thread)
  for index = 0 to threads - 1 do
    whole = list.filter(fun (l) -> l == make_record(index), lines)
    test.equal(list.length(whole), records_per_thread)

  end
  test.equal(temp_files(dir), [])
end

def concurrent(dir) =
  target = path.concat(dir, "concurrent")
  write = file.write.stream(atomic=true, target)
  writers_done = ref(0)
  closers_started = ref(false)
  closers_done = ref(0)

  def writer(index) =
    written = ref(0)
    def run() =
      write(make_record(index) ^ "\n")
      ref.incr(written)
      if
        written() < records_per_thread
      then
        0.
      else
        ref.incr(writers_done)
        (-1.)
      end
    end
    run
  end

  def closer() =
    write(null)
    ref.incr(closers_done)
    (-1.)
  end

  def check() =
    if
      writers_done() < threads
    then
      0.
    elsif
      not closers_started()
    then
      test.equal(file.exists(target), false)
      for _ = 1 to threads do
        thread.run.recurrent(fast=false, delay=0., on_error=null, closer)

      end
      closers_started := true
      0.
    elsif
      closers_done() < threads
    then
      0.
    else
      check_concurrent_writes(dir, target)
      test.pass()
      (-1.)
    end
  end

  for index = 0 to threads - 1 do
    thread.run.recurrent(fast=false, delay=0., on_error=null, writer(index))

  end
  thread.run.recurrent(fast=true, delay=0., on_error=null, check)
end

def f() =
  dir = file.temp_dir(cleanup=true, "file_write_stream")

  # Two atomic writers open in the same directory at once.
  a = path.concat(dir, "a")
  b = path.concat(dir, "b")
  write_a = file.write.stream(atomic=true, a)
  write_b = file.write.stream(atomic=true, b)
  write_a("aaa")
  write_b("bbb")
  test.equal(list.length(temp_files(dir)), 2)
  write_b("")
  write_a("")
  test.equal(file.contents(a), "aaa")
  test.equal(file.contents(b), "bbb")
  test.equal(temp_files(dir), [])

  # Atomic append keeps the existing content.
  write_a = file.write.stream(atomic=true, append=true, a)
  write_a("AAA")
  test.equal(file.contents(a), "aaa")
  write_a("")
  test.equal(file.contents(a), "aaaAAA")

  # Closing again is a no-op, writing after closing fails.
  write_a("")
  test.raises(fun () -> write_a("late"))
  test.equal(file.contents(a), "aaaAAA")

  # Aborting leaves the destination as it was and removes the temporary file.
  write_a = file.write.stream(atomic=true, a)
  write_a("discarded")
  write_a.abort()
  test.equal(file.contents(a), "aaaAAA")
  test.equal(temp_files(dir), [])

  # A raising `data` getter does not publish what was written before.
  chunks = ref(0)
  def failing_data() =
    ref.incr(chunks)
    if
      chunks() > 1
    then
      error.raise(
        error.not_found,
        "no more data"
      )
    end
    "partial"
  end
  test.raises(fun () -> file.write(atomic=true, data=failing_data, a))
  test.equal(file.contents(a), "aaaAAA")
  test.equal(temp_files(dir), [])

  # A failed rename reaches the caller with its message.
  gone = file.temp_dir("file_write_stream_gone")
  write_gone = file.write.stream(atomic=true, path.concat(gone, "x"))
  write_gone("data")
  file.rmdir(gone)
  message =
    try
      write_gone("")
      ""
    catch err do
      err.message
    end
  test.not.equal(message, "")

  # Atomic writes honour perms.
  p = path.concat(dir, "perms")
  file.write(atomic=true, perms=0o640, data="x", p)
  mode =
    string.sub(
      process.read(
        "ls -l #{process.quote(p)}"
      ),
      start=0,
      length=10
    )
  test.equal(mode, "-rw-r-----")

  concurrent(dir)
end

test.check(f)
