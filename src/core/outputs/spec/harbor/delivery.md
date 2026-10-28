# Delivery

How bytes owed to listeners reach their sockets. This machinery is common to
both modes; what a listener is owed and how it is staged is the mode's
([modes.md §3–§4](modes.md#3-pending-test)).

**Goals.** Delivery is built for minimal memory and CPU use and low latency:

- per-listener memory is bounded by `buffer` plus the newest encoder output,
  the initial bytes and ICY blocks, independent of uptime and stream length;
- the cost of an idle output (no listener) is independent of the machine:
  of its core count, of the scheduler's domain count, of the number of
  delivery groups;
- the cost per streaming cycle on the streaming clock is independent of the
  number of listeners in shared mode;
- time to first byte is bounded by admission and one delivery pass
  ([connection.md §6](connection.md#6-admission));
- delivery adds no threads: its work runs on the scheduler's existing
  domains, so process memory does not grow with thread-local allocator state
  multiplied per output.

## 1. Delivery groups

Listeners are partitioned into **delivery groups**. Each listener belongs to
exactly one group for its whole life. A group is served by one **delivery
pass** at a time; passes of different groups are independent and MAY run in
parallel. How many groups an output has, and how listeners are assigned to
them, is the implementation's choice; it MAY follow
`settings.harbor.output.writers` ([operator.md §6](operator.md#6-settings)).
Where passes run, and how many run at once, is the scheduler's decision
([contracts.md §5](contracts.md#5-scheduler)).

A delivery pass is a **fast I/O task** in the scheduler's non-blocking
class: it performs only non-blocking socket writes and memory copies, never
waits on a socket, a lock held across I/O, or user code, and never parks a
domain. It waits on its group's wake-up and, while any member is owed bytes,
on a retry delay.

## 2. Wake-ups

Waking a group schedules one delivery pass for it. Wake-ups MUST coalesce (a
group already due for a pass is not scheduled twice) and MUST NOT block the
caller: the streaming clock wakes groups on every cycle.

**Who wakes.**

- _Every streaming cycle_, after the mode has made the cycle's encoded data
  available ([modes.md §5](modes.md#5-encode-and-send)): every group that has
  at least one member.
- _Admission_: the group a listener is published to.
- _Disconnection_: the listener's group, so that it is removed promptly
  ([§6](#6-disconnection)).
- _Progress_: a group whose pass left a member owed bytes after that
  member's socket accepted some is woken again at once. The socket may have
  room for more, and waiting for the next cycle would cap a listener's rate
  at one write per cycle, below the stream's rate when outputs are large.
  Passes stop following each other once no member's socket accepts bytes.
- _Retry_: a group with a member still owed bytes after a pass is woken
  again after at most `RETRY_INTERVAL` (recommended 1 s) when no cycle wakes
  it first. This keeps slow listeners and timeouts progressing when the
  stream produces no data for a while.

A group with no members is never woken and runs no pass.

Writability is discovered by trying a non-blocking write on each pass. An
implementation MAY in addition wait for writability of sockets that are owed
bytes, but MUST NOT wait on sockets owed nothing: those are always writable
and the wait would spin. Delivery MUST make progress where readiness is
reported only once per edge, as some platforms do once descriptors of
another kind share the wait set.

## 3. The delivery pass

1. Take `now` (monotonic clock).
2. For each member, in any order: if it is not marked closed and the mode says
   it is owed bytes ([modes.md §3](modes.md#3-pending-test)):
   1. flush it ([modes.md §4](modes.md#4-flush));
   2. if it is still not marked closed and `now − last progress > timeout`,
      log `Listener <id> timed out (no progress for <timeout>s)` and mark it
      closed ([§6](#6-disconnection)).
3. Remove the closed members and close their sockets ([§8](#8-removal-and-socket-close)).
4. If a member is still owed bytes, arm the retry ([§2](#2-wake-ups)).

A failure while serving one member MUST be confined to that member: it is
logged and the member is marked closed, and the pass continues with the
others. A failure that recurs on every pass would otherwise starve the whole
group with no listener ever timed out.

## 4. Writing to a socket

A write offers a byte range to the listener's socket through its transport
and classifies the outcome:

| Outcome                                     | Result | Effect                                               |
| ------------------------------------------- | ------ | ---------------------------------------------------- |
| `k > 0` bytes accepted                      | `k`    | last progress := now; `bytes_sent += k`              |
| 0 bytes accepted, would block, or `ENOBUFS` | 0      | none                                                 |
| `EPIPE`, `ECONNRESET` or `ECONNABORTED`     | −1     | log `Socket write for <id>: <error> (disconnecting)` |
| any other error                             | −1     | log `Socket write error for <id>: <error>`           |

"Would block" is `EAGAIN` or `EWOULDBLOCK`, whichever the platform reports.
`ENOBUFS` is a transient shortage of socket buffers that some platforms report
under load; it is retried like a full socket, and the no-progress timeout
bounds it. A peer that closed its end is reported as `EPIPE` or `ECONNRESET`
on some platforms and as `ECONNABORTED` on others.

A result of −1 makes the mode mark the listener closed. One flush issues at
most one write. Every transport MUST offer this non-blocking write, encrypted
ones included ([contracts.md §1](contracts.md#1-harbor-server)): a write that
blocks until the whole range is sent would hold the pass, and every member of
the group, behind one slow peer.

## 5. No-progress timeout

`last progress` starts at the listener's publication and is updated on every
write that accepts at least one byte. A listener is timed out only while it
is owed bytes, right after a flush attempt. A listener owed nothing is never
timed out, and one whose socket accepts any byte within `timeout` is never
timed out. With the retry of [§2](#2-wake-ups), a stalled listener is closed
at most `RETRY_INTERVAL` after its `timeout` elapses.

## 6. Disconnection

**Causes.** A write error ([§4](#4-writing-to-a-socket)); overflow (the mode
finds the listener more than `buffer` behind, logged `Listener <id> buffer
overflow (client too slow), disconnecting`); the no-progress timeout; a
failure while serving it ([§3](#3-the-delivery-pass)); a failed admission
([connection.md §7](connection.md#7-failure-paths)); the output stopping;
process shutdown ([operator.md §7](operator.md#7-lifecycle)).

A listener that falls behind is disconnected, never trimmed: dropping owed
bytes would corrupt the audio and, with ICY, every later boundary
([icy.md §4](icy.md#4-cadence-and-change-detection)).

A client closing its end is noticed through a write error. Every listener of
a streaming output is owed bytes each cycle, so this happens within a cycle
while the stream runs.

**Marking closed** is the only operation a cause performs. It MUST be
idempotent and race-free: exactly one caller wins. The winner records the
disconnection time, logs `Listener <id> disconnected`, and wakes the
listener's group. After marking, no new byte is staged for the listener.

**Teardown** runs once per listener, after its socket is closed: by its group
on removal ([§8](#8-removal-and-socket-close)), or by admission for a listener
that never reached a running group. It is work allowed to block, since it
runs encoder code and user callbacks:

1. the mode's listener teardown ([modes.md §9](modes.md#9-listener-teardown)),
   logging `Listener <id> encoder stop failed: …` if it raises;
2. the `on_disconnect` handlers, with `duration` measured to the
   disconnection time.

Running teardown after removal makes `bytes_sent` final when `on_disconnect`
reads it: no write can be in progress for a listener its group has removed.

## 7. Stopping

On output stop, after the encoder's final bytes are queued
([operator.md §7](operator.md#7-lifecycle)):

1. Every group is told to stop. No listener is published to a group after
   that point succeeds silently: a publication that finds its group stopping
   marks the listener closed and closes its socket itself
   ([connection.md §6](connection.md#6-admission)).
2. Each group runs one last pass: one flush attempt for every member owed
   bytes, with no wait and no retry. This is where listeners receive the
   final bytes, when their sockets have room.
3. Each group marks every member closed, removes it and closes its socket,
   then ends.

Publication and the stop of a group MUST be ordered so that every listener is
either seen by step 3 or sees the stop at publication; with both checks in
place, no listener is left in an ended group.

## 8. Removal and socket close

At the end of each pass, the group removes its members marked closed and
closes their sockets, then queues each removed listener's teardown
([§6](#6-disconnection)).

A listener's socket MUST be closed exactly once, by whoever owns it: its
group once it is published, admission before that. It MUST NOT be closed
while another task may still use its descriptor: a closed descriptor number
can be reused by a new connection, and a late write or wait would then hit
an unrelated peer. Errors on close are ignored.
