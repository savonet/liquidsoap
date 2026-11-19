%include "../test.liq"

def f() =
  tmp = file.temp("foo", "bla")
  on_cleanup({file.remove(tmp)})
  s = sine()
  insert_metadata = s.insert_metadata

  # An output fault: a file the output cannot create, under a path whose
  # parent is a regular file. Script code that raises is no fault of the
  # output's: it is logged and the stream goes on.
  unwritable = "#{tmp}/out.wav"
  broken = ref(false)
  filename = {broken() ? unwritable : tmp}
  callstack = ref([])

  reopen_when_ref = ref(false)

  def reopen_when() =
    r = reopen_when_ref()
    reopen_when_ref := false
    r
  end

  def reopen_on_metadata(_) =
    print(
      "Got metadata!"
    )
    callstack := [...(callstack()), "metadata"]
    false
  end

  def reopen_on_error(_) =
    print(
      "Got error!"
    )
    callstack := [...(callstack()), "error"]
    null
  end

  clock.assign_new(
    on_error=fun (_) ->
      begin
        callstack :=
          [
            ...(callstack()),
            "streaming error"
          ]
      end,
    [s]
  )

  output.file(
    reopen_on_metadata=reopen_on_metadata,
    reopen_on_error=reopen_on_error,
    reopen_when=reopen_when,
    reopen_delay=0.,
    %wav,
    filename,
    s
  )

  thread.run(delay=1., {insert_metadata([("foo", "bla")])})

  # At 5 s the output is asked to reopen onto a file it cannot create, and
  # `reopen_on_error` says not to retry: the error reaches the clock.
  thread.run(
    delay=5.,
    {
      broken := true
      reopen_when_ref := true
    }
  )
  thread.run(
    delay=6.,
    {
      if
        callstack() ==
          [
            "metadata",
            "error",
            "streaming error"
          ]
      then
        test.pass()
      else
        test.fail()
      end
    }
  )
end

test.check(f)
