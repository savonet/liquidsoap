predicate = ()

# Detect when a predicate becomes true.
# @category Programming
# @param ~init Detect at beginning.
# @param p Predicate.
def predicate.activates(~init=false, p)
  last = ref(not init)
  fun () -> begin
    cur = p()
    ans = (not last()) and cur
    last := cur
    ans
  end
end

# Become true once every time a predicate is true.
# @category Programming
# @param p Predicate.
def predicate.once(p)
  predicate.activates(init=true, p)
end

# Limit the number of times a predicate is true is a row.
# @category Programming
# @param n Number of times the predicate is allowed to be true.
# @param p Predicate.
def predicate.at_most(n, p)
  k = ref(0)
  fun () -> begin
    if p() then
      ref.incr(k)
      k() <= n
    else
      k := 0
      false
    end
  end
end

# Detect when a predicate changes.
# @category Programming
# @param p Predicate.
def predicate.changes(p)
  last = ref(p())
  fun () -> begin
    cur = p()
    ans = (cur != last())
    last := cur
    ans
  end
end

# First occurrence of a predicate.
# @category Programming
# @param p Predicate.
def predicate.first(p)
  done = ref(false)
  fun () -> begin
    if done() then
      false
    else
      if p() then
        done := true
        true
      else
        false
      end
    end
  end
end

# Predicate which is true when a signal is sent. The returned predicate has a
# method `signal` to send the signal.
# @category Programming
# @method signal Send a signal.
def predicate.signal()
  state = ref(false)
  def signal()
    state := true
  end
  def p ()
    if state() then
      state := false
      true
    else
      false
    end
  end
  p.{ signal=signal }
end
