# Run a function in a separate thread.
# @category Programming
# @param ~fast Whether the thread is supposed to return quickly or not. Typically, blocking tasks (e.g. fetching data over the internet) should not be considered to be fast. When set to `false` its priority will be lowered below that of request resolutions and fast timeouts. This is only effective if you set a dedicated queue for fast tasks, see the "scheduler" settings for more details.
# @param ~delay Delay (in seconds) after which the thread should be launched.
# @param ~every How often (in seconds) the thread should be run. If negative or `null`, run once.
# @param ~on_error Error callback executed when an error occurred while running the given function. When passed, \
#                  all raised errors are silenced unless re-raised by the callback.
# @param f Function to execute.
def replaces thread.run(~fast=true, ~delay=0., ~on_error=null, ~every=null, f) =
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

  thread.run.recurrent(fast=fast, delay=delay, on_error=on_error, f)
end

# Unless told not to, we want to batch thread.when calls when the getter is a fixed
# value. We batch them by getter value. All callbacks are expected to be fast so we
# do a single global callback for all batched calls. After tasks_count_warning is
# reached that they might need to refactor their code.
let thread.when = {tasks_count=ref(0), tasks_count_warning=70, batches=ref([])}

# @flag hidden
def thread.when.add_to_batch(~every, check) =
  current_batches = thread.when.batches()

  if
    list.assoc.mem(every, current_batches)
  then
    tasks = list.assoc(every, current_batches)
    tasks := [check, ...tasks()]
  else
    tasks = ref([check])
    errors = ref([])
    def exec_task(fn) =
      try
        fn()
      catch err do
        errors := [err, ...errors()]
      end
    end
    def check_tasks() =
      current_tasks = tasks()
      list.iter(exec_task, current_tasks)
      current_errors = errors()

      errors := []

      if
        current_errors != []
      then
        errors_list =
          string.concat(
            separator=", ",
            list.map(string, current_errors)
          )
        log.critical(
          label="thread.when",
          "Error(s) while executing batched `thread.when`: #{errors_list}"
        )
      end

      every
    end
    thread.run.recurrent(fast=true, delay=0., on_error=null, check_tasks)
    thread.when.batches := [(every, tasks), ...current_batches]
  end
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
# @param ~batched When `fast` is `true`, `every` is a fixed value, `on_error` is `null` and `once` is `false`, \
#                 we batch callbacks to make it more efficient. Set this argument to `false` to disable that.
# @param ~on_error Error callback executed when an error occurred while running the given function. When passed, \
#                  all raised errors are silenced unless re-raised by the callback.
# @param p Predicate indicating when to execute the function, typically a time interval such as `{10h-10h30}`.
# @param f Function to execute when the predicate is true.
def thread.when(
  ~fast=true,
  ~init=true,
  ~every=getter(0.5),
  ~once=false,
  ~changed=true,
  ~on_error=null,
  ~batched=true,
  p,
  f
) =
  ref.incr(thread.when.tasks_count)
  if
    thread.when.tasks_count() == thread.when.tasks_count_warning + 1
  then
    log.critical(
      label="thread.when",
      "There are over #{thread.when.tasks_count_warning} `thread.when` tasks \
       running! You might want to consider refactoring your script to use a \
       single callback!"
    )
  end

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

  if
    (
      batched
    and
      getter.is_constant(every)
    and
      not null.defined(on_error)
    and
      fast
    and
      not once
    )
  then
    thread.when.add_to_batch(every=getter.get(every), fun () -> ignore(check()))
  else
    thread.run.recurrent(fast=fast, delay=0., on_error=on_error, check)
  end
end

# @flag hidden
def default_error_handler(~backtrace, err) =
  log(
    label="runtime",
    level=1,
    "Uncaught error #{err}!\n#{backtrace}"
  )
end

thread.on_error(null, default_error_handler)
