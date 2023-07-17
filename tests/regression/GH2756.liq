def f() =
  tmp = file.temp("foo", "bla")
  on_cleanup({file.remove(tmp)})

  s = insert_metadata(sine())
  insert_metadata = s.insert_metadata

  err = ref(null())
  def on_frame() =
    null.case(err(), {()}, fun (e) -> begin
      err := null()
      error.raise(e)
    end)
  end
  s = source.on_frame(s, on_frame)

  reopen_when_ref = ref(true)
  reopen_delay = ref(120.)
  callstack = ref([])

  def reopen_on_metadata(_) =
    print("Got metadata!")
    callstack := [...(callstack()), "metadata"]
    true
  end

  def reopen_on_error(_) =
    print("Got error!")
    callstack := [...(callstack()), "error"]
    2.
  end

  def reopen_when() =
    r = reopen_when_ref()

    if r then
      print("Got reopen!")
      callstack := [...(callstack()), "reload"]
      reopen_when_ref := false
    end

    r
  end

  output.file(reopen_on_metadata=reopen_on_metadata,
              reopen_on_error=reopen_on_error,
              reopen_when=reopen_when,
              reopen_delay=reopen_delay,
              %wav,tmp,s)

  thread.run(delay=1., {insert_metadata([("foo", "bla")])})
  thread.run(delay=5., {err := error.failure})
  thread.run(delay=8., {
    reopen_delay := 1.
    reopen_when_ref := true
  })
  thread.run(delay=10., {
    if callstack() == ["metadata","error","reload"] then test.pass() else test.fail() end
  })
end

test.check(f)
