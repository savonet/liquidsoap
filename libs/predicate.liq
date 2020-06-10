predicate = ()

# Detect when a predicate becomes true.
# @category Liquidsoap
# @flag extra
# @param ~init Detect at begining.
# @param p Predicate.
def predicate.activates(~init=false, p)
  last = ref(not init)
  fun () -> begin
    cur = p()
    ans = (not !last) and cur
    last := cur
    ans
  end
end

# Detect when a predicate changes.
# @category Liquidsoap
# @flag extra
# @param p Predicate.
def predicate.changes(p)
  last = ref(p())
  fun () -> begin
    cur = p()
    ans = (cur != !last)
    last := cur
    ans
  end
end

# First occurrence of a predicate.
# @category Liquidsoap
# @flag extra
# @param p Predicate.
def predicate.once(p)
  done = ref(false)
  fun () -> begin
    if !done then
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
