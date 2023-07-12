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
  reload_in = ref(-1.)
  callstack = ref([])

  def should_reopen(~error, ~metadata) =
    if time() < can_be_called_after() then
      test.fail()
    end

    if null.defined(metadata) then
      print("Got metadata!")
      can_be_called_after := time() + 3.
      callstack := [...(callstack()), "metadata"]
      3.
    elsif null.defined(error) then
      print("Got error!")
      can_be_called_after := time() + 2.
      callstack := [...(callstack()), "error"]
      2.
    else
      r = reload_in()

      if 0. <= r then
        print("Got reopen!")
        callstack := [...(callstack()), "reload"]
        reload_in := -1.
      end

      can_be_called_after := time() + r
      r
    end
  end

  output.file(should_reopen=should_reopen,%wav,tmp,s)

  thread.run(delay=1., {insert_metadata([("foo", "bla")])})
  thread.run(delay=5., {err := error.failure})
  thread.run(delay=8., {reload_in := 1.})
  thread.run(delay=10., {begin
    if callstack() == ["metadata","error","reload"] then test.pass() else test.fail() end
  end})
end

test.check(f)
