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

  can_be_called_after = ref(0.)
  callstack = ref([])

  def should_reopen(~error, ~metadata) =
    if time() < can_be_called_after() then
      test.fail()
    end

    if null.defined(metadata) then
      print("Got metadata!")
      callstack := [...(callstack()), "metadata"]
    elsif null.defined(error) then
      print("Got error!")
      callstack := [...(callstack()), "error"]
    end

    (-1.)
  end

  clock.assign_new(on_error=fun (_) -> begin
    callstack := [...(callstack()), "streaming error"]
  end, [s])

  output.file(should_reopen=should_reopen,%wav,tmp,s)

  thread.run(delay=1., {insert_metadata([("foo", "bla")])})
  thread.run(delay=5., {err := error.failure})
  thread.run(delay=6., {begin
    if callstack() == ["metadata","error","streaming error"] then test.pass() else test.fail() end
  end})
end

test.check(f)
