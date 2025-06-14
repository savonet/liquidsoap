def f() =
  retry_delay_called_count = ref(0)

  def retry_delay() =
    ref.incr(retry_delay_called_count)
    0.1
  end

  last_call = ref(null)
  def next() =
    t = time()
    called_at = last_call()
    last_call := t

    if
      null.defined(called_at)
    then
      if t - null.get(called_at) < 0.1 then test.fail() end
      if 2 < retry_delay_called_count() then test.pass() end
      request.create("invalid")
    else
      null
    end
  end

  s = request.dynamic(retry_delay=retry_delay, next)

  output.dummy(fallible=true, s)
end

test.check(f)
