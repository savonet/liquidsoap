log.level := 4
settings.log.level.lifecycle := 5

test = ()

let test.done = ref(false)

runtime.gc.set(runtime.gc.get().{space_overhead=20, allocation_policy=2})

# End test successfully.
def test.pass()
  if not test.done() then
    test.done := true
    print("Test passed!")
    shutdown()
  end
end

# End test with a failure.
def test.fail(reason = null())
  reason = if null.defined(reason) then ": #{null.get(reason)}" else "!" end
  print("Test failed#{reason}")
  exit(1)
end

# End test with signal 2
def test.skip()
  print("Test skipped..")
  exit(2)
end

# Check that files are never repeated in source s, possibly by rounds. The
# function triggers test.fail on repeated filenames, only clearing its list of
# seen filenames once all nb_files have been seen.
def test.check_non_repeating(~nb_files,~nb_rounds,s)
  # List of seen filenames
  seen = ref([])
  # Number of rounds to test
  iterations = ref(0)

  def already(fname)
    list.assoc(default=false, fname, seen())
  end

  def check(m)
    fname = m["filename"]
    print("I: Playing #{fname}")
    if iterations() < nb_rounds and already(fname) then
      print("I: Already seen #{fname}")
      test.fail()
    else
      if list.length(seen()) < nb_files-1 then
        seen := list.add((fname,true),seen())
      else
        print("I: ===")
        seen := []
        iterations := iterations()+1
        if iterations() == nb_rounds then
          print("I: Test passed")
          test.pass()
        end
      end
    end
  end

  s.on_track(check)

  s
end

def test.equal(v, v') =
  if v != v' then
    msg = "expected:\r\n#{string.quote(string(v'))}\r\ngot:\r\n#{string.quote(string(v))}"
    print(msg)
    error.raise(error.failure, msg)
    test.fail()
  end
end

let test.not = ()

def test.not.equal(v, v') =
  if v == v' then
    msg = "expected:\r\n#{string.quote(string(v'))}\r\nto differ from:\r\n#{string.quote(string(v))}"
    print(msg)
    error.raise(error.failure, msg)
    test.fail()
  end
end

# Compares two float values for approximate equality.
# Values are considered almost equal if their difference
# is within 10^-digits.
# @param ~digits The number of decimal digits to compare. Default is 7.
# @flag hidden
def test._almost_equal(~digits=7, first, second) =
  diff = abs(float(first) - float(second))
  if
    diff * float(pow(10, digits)) < 0.5
  then
    true.{diff=diff}
  else
    false.{diff=diff}
  end
end

# Compares two float values for approximate equality.
# Values are considered almost equal if their difference
# is within 10^-digits.
# @param ~digits The number of decimal digits to compare. Default is 7.
def test.almost_equal(~digits=7, first, second) =
  is_almost_equal = test._almost_equal(digits=digits, first, second)
  if
    not is_almost_equal
  then
    error.raise(
      error.failure,
      "#{string.quote(string(first))} != #{string.quote(string(second))} \
       up to #{digits} digits, diff = #{is_almost_equal.diff}."
    )

    test.fail()
  end
end

# Compares two float values for approximate inequality.
# Values are considered not almost equal if their difference
# is greater than 10^-digits.
# @param ~digits The number of decimal digits to compare. Default is 7.
def test.not_almost_equal(~digits=7, first, second) =
  is_almost_equal = test._almost_equal(digits=digits, first, second)
  if
    is_almost_equal
  then
    error.raise(
      error.failure,
      "#{string.quote(string(first))} == #{string.quote(string(second))} \
       up to #{digits} digits, diff = #{is_almost_equal.diff}."
    )

    test.fail()
  end
end

# Asynchronous test handler with dummy output
# Best practice is to run all manual tests
# through this one.
def test.check(f) =
  thread.run(delay=0.1,f)
  output.dummy(blank())
end

def test.raise(fn) =
  try ignore(fn()) catch _ do () end
end

def on_error(~backtrace, error) =
  print("Uncaught error while running test: #{error}\n#{backtrace}")
  test.fail()
end
thread.on_error(null(), on_error)
