# Duppy scheduler specification

This document states what a duppy scheduler does. It is normative and independent of any implementation: an implementation is correct when it satisfies every statement here, whatever its data structures.

The words MUST, MUST NOT, SHOULD and MAY are used as in RFC 2119.

## 1. Model

A **scheduler** owns a set of tasks and a pool of workers.

A **task** is a set of events to wait for, a priority, and a handler to run once at least one of the events has occurred.

A **worker** is an execution context that runs handlers. The **pool** is the set of workers, fixed between start and stop.

The **core** is the part of the scheduler that tracks waiting tasks, detects their events and hands ready tasks to workers. The **host** is the language runtime in which handlers run.

## 2. Tasks and events

### 2.1 Events

- `Delay d`: `d` seconds have elapsed since the task was submitted.
- `Read fd`: `fd` can be read without blocking.
- `Write fd`: `fd` can be written without blocking.

Delays MUST be measured on a monotonic clock. A change of the wall-clock time MUST NOT move a deadline.

A descriptor in error or hung up satisfies a `Read` or `Write` wait on it, so that the handler meets the error in its own I/O.

Descriptor readiness is level-triggered: a condition that still holds when a task is submitted satisfies that task.

### 2.2 Life of a task

A task goes through these states, in order:

1. **Waiting**: submitted, none of its events has occurred.
2. **Ready**: at least one event has occurred; no worker has taken it.
3. **Running**: a worker is executing its handler.
4. **Done**: the handler has returned.

A task MUST become ready at most once and its handler MUST run at most once.

The handler receives the events that had occurred when the task became ready. That list MUST be non-empty and MUST contain only events of the task.

A task submitted with a delay that has already elapsed, zero included, MUST become ready at submission, in the context that submits it.

A task waiting on a descriptor that cannot be watched MUST become ready at once, with the events of that descriptor reported as occurred. Events on its other descriptors MUST NOT be reported.

The tasks returned by a handler are submitted when it returns, as if by the worker that ran it.

### 2.3 Deadlines

A `Delay` MUST NOT be reported before its deadline.

Tasks whose deadlines have passed become ready in deadline order; tasks sharing a deadline become ready in submission order.

### 2.4 Parked computations

A handler MAY park its remaining work. The worker is given back, and the remaining work becomes a task of the priority given at that moment. It resumes on any worker eligible for that priority.

- Parked on events, the work resumes when one of them occurs and receives those that did.
- Parked on a resumer, the work hands out a function that makes it ready. The function MAY be called from any thread. Given a delay, the work also becomes ready once the delay has elapsed. The work MUST resume exactly once, on whichever comes first, and every later call to the resumer does nothing.
- Parked on a condition, the work resumes once what it waits for holds. Whoever changes what a waiter checks signals the condition, and every waiter checks again. A wait from a thread outside any handler blocks that thread until the condition holds. A signal sent between a waiter's check and its park MUST reach that waiter.

## 3. Priorities

Each task has a priority. The scheduler maps every priority to an integer **rank** by a function fixed at creation. A lower rank is more urgent.

The mapping MUST be pure: the same priority always has the same rank. An implementation MAY bound the number of distinct ranks, and MUST support at least 16. It MAY bound the number of descriptors one task waits on, and MUST support at least 16.

## 4. Classes

The scheduler maps every priority to one of three classes by a function fixed at creation.

- **Immediate**: the handler does not block and is short. It runs on the worker itself, as part of a batch (6.2).
- **Direct**: the handler does not block and its duration is bounded and known to the application. It runs on the worker itself, alone.
- **Threaded**: the handler may block or run for an unbounded time. It runs so that the worker is free to dispatch other tasks while it is parked (7).

While a worker runs an Immediate batch or a Direct task it dispatches nothing else. The scheduler cannot interrupt a handler: the latency of every task a worker could have taken is bounded below by the handlers it is running. Keeping Immediate and Direct handlers short is the application's duty.

## 5. Workers and eligibility

The workers of a pool run handlers in parallel with each other. By default any of them can run any task.

A worker MAY be given, when the pool starts, a rule saying which priorities it **accepts**. The rule MUST be pure: the same priority is always accepted or always declined by that worker. A worker given no rule accepts every priority.

A task MAY be **pinned** to one worker. A task is **eligible** for a worker if that worker accepts its priority and the task is not pinned to another one.

A task pinned to a worker MUST only run on that worker, and so MUST every task its handler returns.

Submitting a task pinned to a worker that does not exist, or that does not accept it, MUST fail at submission.

A task that is eligible for no worker is never run. Giving every priority a worker that accepts it is the application's duty.

The scheduler MUST work with a single worker. No rule in this document may be satisfied only by having a second one.

An implementation MAY, at its own discretion, also offer a pool whose workers do not run in parallel. Such a pool is outside this specification.

### 5.1 Threads of their own

A caller MAY ask the pool for a thread of its own, for work that blocks for an unbounded time. Such a thread holds neither a worker nor a slot of the blocking budget (7).

The thread MUST be placed on the domain of the worker that accepts the given priority and has the fewest such threads. The count includes a thread from the moment its place is chosen, so that two requests in a row go to two workers.

Before the pool starts, and on a pool whose workers do not run in parallel, the thread is created where the request is made.

## 6. Dispatch

### 6.1 Order

Among the ready tasks of one class and one rank that are eligible for a worker, the worker MUST take the one that became ready first.

Among ready Direct and Threaded tasks eligible for a worker, the worker MUST take one of the lowest rank. On equal ranks a Direct task goes first.

A lower rank always goes first: a ready list that never empties of rank `r` starves every rank above `r`. This is intended.

### 6.2 Batches

When a worker takes Immediate tasks, it takes the ready Immediate tasks eligible for it as one batch, and runs them in the order they became ready. An implementation MAY bound the size of a batch; the tasks left behind stay ready, in order, and MUST be offered to an idle worker if there is one.

A worker MUST alternate: after a batch, if a Direct or Threaded task is ready, eligible and can be taken (7), it takes that task before another batch. Immediate tasks that become ready as fast as they are run therefore cannot starve the other classes, even on a single worker.

### 6.3 Spreading

Ready Direct and Threaded tasks MUST be taken one per worker, so that they spread over the pool instead of queueing behind one worker while others are idle.

### 6.4 Liveness

If a task is ready and a worker eligible for it is idle and able to take it, that worker MUST take it without waiting for an unrelated event.

A worker with nothing to take MUST NOT consume CPU while idle.

## 7. Blocking budget

`max_blocking` is the largest number of Threaded tasks that may be running at once. It is set when the pool starts.

The budget belongs to the pool as a whole: a running Threaded task holds one **slot** of it, whichever worker took it. While every slot is in use no worker may take a Threaded task; they all still take Immediate and Direct ones. A budget below one counts as one.

So that Threaded tasks spread over the pool (6.3), a worker MUST leave one to another worker that is eligible for it, is not running a handler and has fewer Threaded tasks running. A worker MAY therefore hold more than an even share of the budget while the others are busy.

A Threaded task that cannot be taken for lack of a slot stays ready, keeps its place (6.1), and MUST be taken by a worker eligible for it once a slot frees up.

A caller MAY reserve one slot beyond the budget and later give it back. A lowered budget takes effect as running tasks return.

## 8. Timing

The transition from waiting to ready MUST NOT wait for any handler to return.

The transition from waiting to ready MUST NOT be delayed by the host: not by any pause its runtime imposes on running code, not by its locks, not by the progress of any worker.

A waiting task with a deadline `t` MUST be ready no later than `t` plus the time the operating system takes to schedule the core.

## 9. Mutual exclusion

The core's state is shared by every worker. Access to it MUST satisfy:

- No execution context is ever suspended by the host while it holds exclusive access to core state.
- Exclusive access is held for a time bounded by the logarithm of the number of tasks, or by the number of tasks handed over in that access.
- No handler, no host callback and no blocking system call runs under exclusive access.

A worker waiting for work, and the core waiting for events, MUST NOT hold exclusive access while they wait.

## 10. Start and stop

A scheduler accepts tasks before it is started. They wait, or sit ready, until the pool starts.

Starting a scheduler twice MUST fail.

On stop:

- Waiting tasks and ready tasks not yet taken are dropped; their handlers never run.
- Running handlers are allowed to finish, for a bounded time. A Threaded handler is under no obligation to return, so stop MUST NOT wait for one indefinitely.
- Tasks returned by a handler that finishes after stop are dropped.
- Stop returns once the workers have been told to exit and the bounded wait is over.

## 11. Errors

An exception escaping a handler is passed to the scheduler's error callback, in the context that ran the handler. The task counts as done and returns no tasks.

A failure of the core itself is fatal: waiting tasks are dropped and the fatal callback is called from a worker context. The core MUST NOT call into the host to report it.

## 12. Structure

The scheduler is split in two.

### 12.1 The core

The core holds the waiting tasks, the deadlines, the watched descriptors, the ready tasks and the idle workers. It waits for events on its own thread.

**The core MUST NOT depend on the host.** In particular:

- It uses no interface of the host's runtime and links against no part of it.
- It builds and passes its own tests alone, with no toolchain of the host present.
- It holds no value of the host. A task is known to the core by an integer **handle** and by plain data: its rank, class, eligibility, deadline, descriptors and which events occurred.
- Its event thread is an ordinary operating-system thread, unknown to the host's runtime. It never runs code of the host and is never paused by it.
- Its event thread blocks every signal, so that no signal handler of the host runs on it.

### 12.2 The binding

The binding is the only code that knows both sides. It:

- keeps the handlers, in a table indexed by handle;
- evaluates rank, class and eligibility when a task is submitted and passes them to the core as plain data;
- runs handlers, the wrapper and the error callbacks on workers;
- releases the host's locks around every wait on the core, and around nothing else.

### 12.3 Handles

A handle is chosen by the binding and is valid from submission until the task is taken by a worker or dropped. The binding MUST NOT submit a handle that is still valid. The core treats it as opaque.

The binding stores the handler under its handle before submitting the task, and removes it when the task is taken or dropped. A worker that receives a handle from the core therefore always finds its handler.

## 13. Examples

This section is not normative.

### 13.1 Order on one worker

A pool of one worker is idle when these tasks become ready, in this order:

| task | class     | rank |
| ---- | --------- | ---- |
| A    | Immediate | 0    |
| B    | Threaded  | 2    |
| C    | Immediate | 0    |
| D    | Direct    | 2    |
| E    | Direct    | 1    |

The worker takes:

1. A and C, as one batch (6.2).
2. E, the lowest rank among the Direct and Threaded tasks (6.1).
3. D, since a Direct task goes before a Threaded one of the same rank (6.1).
4. B, which runs parked and leaves the worker free (4).

An Immediate task F that becomes ready while E runs is taken after E and before D: the worker alternates between a batch and a single task (6.2).

### 13.2 Blocking budget

A pool of two workers starts with `max_blocking` set to 2 (7). Three Threaded tasks X, Y and Z become ready.

If both workers are free, each takes one of X and Y; if one of them is in a long Direct handler, the other takes both. Z stays ready: the pool has no slot left, and both workers keep taking Immediate and Direct tasks. When X returns, Z is taken, by the worker with fewer Threaded tasks running if both are free.

### 13.3 A deadline under load

A task waits on `Delay 0.02` while every worker runs a Direct handler that lasts 10 ms.

At the deadline the task becomes ready, whatever the workers are doing (8). Its handler starts when the first worker returns, up to 10 ms later: the scheduler does not interrupt a handler (4). Its rank decides only which ready task that worker takes first.
