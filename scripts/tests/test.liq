# End test successfully.
def test.pass()
  print("TEST PASSED")
  shutdown()
end

# End test with a failure.
def test.fail()
  print("TEST FAILED")
  shutdown()
end

# Check that files are never repeated in source s, possibly by rounds.
# The function triggers test.fail on repeated filenames,
# only clearing its list of seen filenames once all nb_files have been seen.
def test.check_non_repeating(~nb_files,~nb_rounds,s)

  seen = ref([])       # List of seen filenames
  iterations = ref(0)  # Number of rounds to test

  def check(m)
    fname = m["filename"]
    cur_seen = !seen
    if cur_seen[fname] != "" then
      test.fail()
    else
      if list.length(!seen) < nb_files-1 then
        seen := list.add((fname,fname),!seen)
      else
        seen := []
        iterations := !iterations+1
        if !iterations == nb_rounds then
          test.pass()
        end
      end
    end
  end

  on_track(id="check_non_repeating",check,s)
end
