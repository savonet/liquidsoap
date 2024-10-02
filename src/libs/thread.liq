# Run a function in a separate thread.
# @category Programming
# @param ~queue Queue to use for the task. Should be one of: `"generic"` or `"non_blocking"`. \
#               Non blocking should be reserved for tasks that are known to complete quickly. \
#               You can also use a dedicated queue name declared via `settings.scheduler.queues`.
# @param ~delay Delay (in seconds) after which the thread should be launched.
# @param ~every How often (in seconds) the thread should be run. If negative or `null`, run once.
# @param ~on_error Error callback executed when an error occurred while running the given function. When passed, \
#                  all raised errors are silenced unless re-raised by the callback.
# @param f Function to execute.
def replaces thread.run(
  ~queue="generic",
  ~delay=0.,
  ~on_error=null(),
  ~every=null(),
  f
) =
  every = every ?? getter(-1.)

  def f() =
    ignore(f() == ())
    getter.get(every)
  end

  on_error =
    null.map(
      fun (fn) ->
        fun (err) ->
          begin
            (fn(err) : unit)
            (-1.)
          end,
      on_error
    )

  thread.run.recurrent(queue=queue, delay=delay, on_error=on_error, f)
end

# Execute a callback when a predicate is `true`. The predicate
# is checked `every` seconds and the callback is
# called when the predicate returns `true` after having been
# `false`, following the same semantics as `predicate.activates`.
# @category Programming
# @param ~queue Queue to use for the task. Should be one of: `"generic"` or `"non_blocking"`. \
#               Non blocking should be reserved for tasks that are known to complete quickly. \
#               You can also use a dedicated queue name declared via `settings.scheduler.queues`.
# @param ~init Detect at beginning.
# @param ~every How often (in sec.) to check for the predicate.
# @param ~once Execute the function only once.
# @param ~changed Execute the function only if the predicate was false when last checked.
# @param ~on_error Error callback executed when an error occurred while running the given function. When passed, \
#                  all raised errors are silenced unless re-raised by the callback.
# @param p Predicate indicating when to execute the function, typically a time interval such as `{10h-10h30}`.
# @param f Function to execute when the predicate is true.
def thread.when(
  ~queue="generic",
  ~init=true,
  ~every=getter(0.5),
  ~once=false,
  ~changed=true,
  ~on_error=null(),
  p,
  f
) =
  p = once or not changed ? p : (predicate.activates(init=init, p))

  def check() =
    if
      p()
    then
      f()
      once ? (-1.) : (getter.get(every))
    else
      getter.get(every)
    end
  end

  thread.run.recurrent(queue=queue, delay=0., on_error=on_error, check)
end

# @flag hidden
def default_error_handler(~backtrace, err) =
  log(
    label="runtime",
    level=1,
    "Uncaught error #{err}!\n#{backtrace}"
  )
end

thread.on_error(null(), default_error_handler)
