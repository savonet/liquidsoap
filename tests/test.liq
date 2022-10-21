log.level.set(4)

test = ()

# End test successfully.
def test.pass()
  print("Test passed!")
  shutdown()
end

# End test with a failure.
def test.fail()
  print("Test failed!")
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
    list.assoc(default=false, fname, !seen)
  end

  def check(m)
    fname = m["filename"]
    print("I: Playing #{fname}")
    if !iterations < nb_rounds and already(fname) then
      print("I: Already seen #{fname}")
      test.fail()
    else
      if list.length(!seen) < nb_files-1 then
        seen := list.add((fname,true),!seen)
      else
        print("I: ===")
        seen := []
        iterations := !iterations+1
        if !iterations == nb_rounds then
          print("I: Test passed")
          test.pass()
        end
      end
    end
  end

  s.on_track(check)

  s
end

def test.equals(v, v') =
  if v != v' then
    print("expected:\r\n#{string.quote(string(v'))}\r\ngot:\r\n#{string.quote(string(v))}")
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
