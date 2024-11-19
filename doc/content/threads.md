# Threads

The main purpose of liquidsoap is to create real time media streams. When streams are created, everything that
is needed to compute them needs to happen very quickly so that we make sure that the stream can in fact
be created in real time.

When a task is required that may take some time and whose result is not required for the stream generation,
for instance when executing a `on_stop` or `on_connect` callback, it can be useful to execute this task in a _thread_.

Threads in liquidsoap are callback functions that are executed by an asynchronous queue. Here's an example:

```{.liquidsoap include="task-example.liq"}

```

By default, there are two type of queues available in liquidsoap:

- `generic` queues
- `non_blocking` queues

By convention, tasks that are known to be executing very fast should be sent to the
`non_blocking` queues and all the other tasks should be sent to the `generic` queue.

You can decide which queue to send tasks to by using the `queue` parameter of the
`thread.run` functions. Some other operators who also use threads can have a similar
parameter such as `thread_queue` for `request.dynamic` and `playlist`.

You can also define your own named queue using the `settings.scheduler.queues` setting.

```{.liquidsoap include="task-with-queue.liq"}

```

This is particularly useful for two applications:

- To control concurrent execution of specific tasks.
- To prevent deadlocks in case some tasks depends on other tasks.
