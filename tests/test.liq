log.level := 4
settings.init.force_start := true

test = ()
let test.done = ref(false)

runtime.gc.set(runtime.gc.get().{space_overhead=20, allocation_policy=2})

# End test successfully.
def test.pass() =
  if
    not test.done()
  then
    test.done := true
    print(
      "Test passed!"
    )
    shutdown()
  end
end

# End test with a failure.
def test.fail(reason=null) =
  reason =
    if
      null.defined(reason)
    then
      ": #{null.get(reason)}"
    else
      "!"
    end
  print(
    "Test failed#{reason}"
  )
  exit(1)
end

# End test with signal 2
def test.skip() =
  print(
    "Test skipped.."
  )
  exit(123)
end

# Check that files are never repeated in source s, possibly by rounds. The
# function triggers test.fail on repeated filenames, only clearing its list of
# seen filenames once all nb_files have been seen.
def test.check_non_repeating(~nb_files, ~nb_rounds, s) =
  # List of seen filenames
  seen = ref([])

  # Number of rounds to test
  iterations = ref(0)

  def already(fname) =
    list.assoc(default=false, fname, seen())
  end

  def check(m) =
    fname = m["filename"]
    print(
      "I: Playing #{fname}"
    )
    if
      iterations() < nb_rounds and already(fname)
    then
      print(
        "I: Already seen #{fname}"
      )
      test.fail()
    else
      if
        list.length(seen()) < nb_files - 1
      then
        seen := list.add((fname, true), seen())
      else
        print(
          "I: ==="
        )
        seen := []
        iterations := iterations() + 1
        if
          iterations() == nb_rounds
        then
          print(
            "I: Test passed"
          )
          test.pass()
        end
      end
    end
  end

  s.on_track(check)
  s
end

def test.equal(v, v') =
  if
    # We compare strings for methods and plain object for type.
    v != v' or "#{v}" != "#{v'}"
  then
    msg =
      "expected:\r\n#{string.quote(string(v'))}\r\ngot:\r\n#{
        string.quote(string(v))
      }"
    print(msg)

    thread.run({test.fail()})

    error.raise(error.failure, msg)
  end
end

let test.not = ()

def test.not.equal(first, second) =
  if
    first == second or "#{first}" == "#{second}"
  then
    msg =
      "expected\r\n
      #{string.quote(string(first))}\r\nto differ from:\r\n#{
        string.quote(string(second))
      }"

    print(msg)

    thread.run({test.fail()})

    error.raise(error.failure, msg)
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
    thread.run({test.fail()})

    error.raise(
      error.failure,
      "#{string.quote(string(first))} != #{
        string.quote(string(second))
      }
       up to #{digits} digits, diff = #{is_almost_equal.diff}."
    )
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
    thread.run({test.fail()})

    error.raise(
      error.failure,
      "#{string.quote(string(first))} == #{
        string.quote(string(second))
      }
       up to #{digits} digits, diff = #{is_almost_equal.diff}."
    )
  end
end

def test.raises(fn, e=null) =
  try
    ignore(fn())
    error.raise(
      error.failure,
      "Exception #{e} was not raised!"
    )
  catch exn do
    if
      null.defined(e)
    then
      e = null.get(e)
      if
        exn.kind != e.kind
      then
        error.raise(
          error.failure,
          "An exception #{exn} of kind #{exn.kind} was raised instead of an \
           error of kind #{e.kind}."
        )
      end
    end

    ()
  end
end

# Asynchronous test handler with dummy output
# Best practice is to run all manual tests
# through this one.
def test.check(f) =
  thread.run(delay=0.1, f)
end

# Add a metric
def test.metric(~category, ~name, ~value, ~unit, ~min_value=null) =
  min_value =
    null.defined(min_value)
    ?
      "\n  min: #{(null.get(min_value) : float)}"
    : ""
  data =
    "- name: \"#{(name : string)}\"\n  category: \"#{(category : string)}\"\n  \
     value: #{(value : float)}\n  unit: \"#{(unit : string)}\"\n  time: #{
      (time() : float)
    }#{min_value}\n\n"

  file.write(append=true, data=data, "/tmp/metrics.yaml")
end

def on_error(~backtrace, err) =
  print(
    "Uncaught error while running test: #{err}\n#{backtrace}"
  )
  test.fail()
end

thread.on_error(null, on_error)
