# Concurrency (Part A)

What every part of Liquidsoap does about threads, the scheduler and shared
state. The clock's use of the scheduler is
[pacing.md §10](../core/clock/spec/pacing.md#10-what-the-clock-asks-of-the-scheduler).

## 1. Work

- Work is handed to the scheduler, which owns domains, threads and placement.
  A component states what work exists, its class and what it waits on; it
  creates no thread of its own unless its document gives the reason.
- Two classes of work. **Fast I/O**: non-blocking, short, never parks a
  domain. **Allowed to block**: everything else. A call that blocks in a
  device or a library holds whatever runs it; it is placed in the second
  class.
- **User code is work allowed to block.** A script function called from
  fast work is moved to the blocking class, or its document says why it
  cannot block.
- Wake-ups coalesce, and asking for one never blocks the caller.

## 2. The streaming thread

- The streaming thread (the one running a clock's tick) never waits for a
  peer, a connection or another thread. A connection is never opened on it.
  The one wait allowed is a blocking sync source's, which is its pacing
  ([pacing.md](../core/clock/spec/pacing.md)).
- No user code runs under an operator lock.
- Every method a script can call MAY be called from any thread, and returns
  without waiting for the streaming thread. What it asks of the stream is a
  request carried out by the streaming thread
  ([source-contract.md §6](../core/source/spec/source-contract.md#6-abort)).
- Script code can group writes in an atomic section. A reader running inside
  one never sees such a group part way through.

## 3. Shared state

- Every piece of state written from more than one thread has one owner and
  atomic transitions.
- A transition that depends on the state checks it in the same atomic step:
  a result that arrives after the state moved on is discarded.
- Reads are never blocked by a writer: a read returns a value the field held
  at some moment.
- Process-wide state is safe to reach from several domains.

## 4. Resources

- Every resource (a socket, a file, an encoder, a native handle) is closed
  exactly once, on every path, including a failure part-way. Closing is
  idempotent.
- A resource is not closed while another task may still use it.
- Marking a resource closed is atomic and race-free; what a closed resource
  is asked afterwards fails with a stated error, never touches freed memory.
