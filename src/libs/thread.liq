# Run a function in a separate thread.
# @category Programming
# @param ~fast Whether the thread is supposed to return quickly or not. Typically, blocking tasks (e.g. fetching data over the internet) should not be considered to be fast. When set to `false` its priority will be lowered below that of request resolutions and fast timeouts. This is only effective if you set a dedicated queue for fast tasks, see the "scheduler" settings for more details.
# @param ~delay Delay (in seconds) after which the thread should be launched.
# @param ~every How often (in seconds) the thread should be run. If negative or `null`, run once.
# @param f Function to execute.
def replaces thread.run(~fast=true, ~delay=0., ~every=null(), f)
  every = every ?? getter(-1.)
  def f() =
    ignore(f() == ())
    getter.get(every)
  end
  thread.run.recurrent(fast=fast, delay=delay, f)
end

# Execute a callback when a predicate is `true`. The predicate
# is checked `every` seconds and the callback is
# called when the predicate returns `true` after having been
# `false`, following the same semantics as `predicate.activates`.
# @category Programming
# @param ~fast Whether the callback is supposed to return quickly or not.
# @param ~init Detect at beginning.
# @param ~every How often (in sec.) to check for the predicate.
# @param ~once Execute the function only once.
# @param ~changed Execute the function only if the predicate was false when last checked.
# @param p Predicate indicating when to execute the function, typically a time interval such as `{10h-10h30}`.
# @param f Function to execute when the predicate is true.
def thread.when(~fast=true, ~init=true, ~every=getter(0.5), ~once=false, ~changed=true, p, f)
  p = once or not changed ? p : (predicate.activates(init=init, p))
  def check()
    if p() then
      f()
      once ? (-1.) : (getter.get(every))
    else
      getter.get(every)
    end
  end
  thread.run.recurrent(fast=fast, delay=0., check)
end

# @flag hidden
def default_error_handler(~backtrace, err) =
  log(label="runtime", level=1, "Uncaught error #{err}!\n#{backtrace}")
end
thread.on_error(null(), default_error_handler)
