# Run a function in a separate thread.
# @category Control
# @param ~fast Whether the thread is supposed to return quickly or not. Typically, blocking tasks (e.g. fetching data over the internet) should not be considered to be fast. When set to `false` its priority will be lowered below that of request resolutions and fast timeouts. This is only effective if you set a dedicated queue for fast tasks, see the "scheduler" settings for more details.
# @param ~delay Delay (in seconds) after which the thread should be lauched.
# @param ~every How often (in seconds) the thread should be run (negative means once).
# @param f Function to execute.
def replaces thread.run(~fast=true, ~delay=0., ~every=float_getter(-1.), f)
  every = to_float_getter(every)
  def f() =
    ignore(f() == ())
    every()
  end
  thread.run.recurrent(fast=fast, delay=delay, f)
end

# Execute a given action when a predicate is true.
# @category System
# @param ~every How often (in sec.) to check for the predicate.
# @param ~once Execute the function only once.
# @param ~changed Execute the function only if the predicate was false when last checked.
# @param p Predicate indicating when to execute the function, typically a time interval such as `{10h-10h30}`.
# @param f Function to execute when the predicate is true.
def thread.when(~fast=true, ~every=1., ~once=false, ~changed=true, p, f)
  last = ref(false)
  def check()
    b = p()
    if b and not (changed and !last) then f() end
    last := b
    if b and once then (-1.) else every end
  end
  thread.run.recurrent(fast=fast, delay=0., check)
end
