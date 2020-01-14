# Detect when a predicate becomes true.
# @category liquidsoap
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
