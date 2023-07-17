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

  callstack = ref([])

  def reopen_on_metadata(_) =
    print("Got metadata!")
    callstack := [...(callstack()), "metadata"]
    false
  end

  def reopen_on_error(_) =
    print("Got error!")
    callstack := [...(callstack()), "error"]
    null()
  end

  clock.assign_new(on_error=fun (_) -> begin
    callstack := [...(callstack()), "streaming error"]
  end, [s])

  output.file(reopen_on_metadata=reopen_on_metadata,
              reopen_on_error=reopen_on_error,
              %wav,tmp,s)

  thread.run(delay=1., {insert_metadata([("foo", "bla")])})
  thread.run(delay=5., {err := error.failure})
  thread.run(delay=6., {
    if callstack() == ["metadata","error","streaming error"] then test.pass() else test.fail() end
  })
end

test.check(f)
