# The connection-role contract

A role owns the SRT sockets of one operator: how they are created, connected,
listed, lost and closed. The operator kind ([input.md](input.md),
[output.md](output.md)) asks for the operations below and reacts to the
events. [caller.md](caller.md) and [listener.md](listener.md) answer every
section under the same number; a role that leaves a clause unanswered is
incomplete.

## 1. Parameters

Each role states which of [common.md §1](common.md#1-parameters) it reads.

## 2. State

**Wanted.** The operator wants a connection when it is started, not held by
`disconnect()`, and the process is not shutting down
([common.md §5](common.md#5-connection-wanted)). Every role activity follows
this one predicate: while it holds, the role connects or listens; when it
stops holding, the role releases everything (§8).

**Peer.** A connected socket the operator exchanges data with, with the
remote address it reached or came from. A peer is **live** from the end of
its `on_connect` (§5) until the role ends it (§8). Data flows only to and
from live peers.

Each role keeps its state as one value per operator, changed by atomic
transitions. A transition that depends on the state (an attempt completing,
an accept landing, a peer failing) MUST check the state in the same atomic
step: a result that arrives after the state moved on is discarded, its
socket closed, and no `on_connect` or `on_disconnect` fires for it.

## 3. Socket setup

Every socket a role creates goes through these steps, in order:

1. Create a libsrt socket.
2. Set transport type to live. It comes first because the library resets
   other options to the transport type's defaults when it is set.
3. Set payload size to `payload_size`.
4. Set the message API flag to `messageapi`.
5. Set enforced encryption to `settings.srt.enforced_encryption`.
6. Set the role's own options (caller §3, listener §3).
7. Fire `on_socket` with the creation mode (`"connect"`, `"listen"` or
   `"incoming"`).

`on_socket` comes after every option the operator sets, so a script can read
them and override any of them. It comes before the socket connects, binds or
carries data, so options negotiated at handshake time can still be set.

**Closing.** Every socket that received a creation event MUST receive
exactly one `"close"` event, fired just before the role closes it, whatever
the path: normal end, failure, stop, shutdown. A socket closed before its
creation event fires receives none. Every socket a role creates MUST be
closed exactly once, on every path, including when a later setup step
fails.

## 4. Connect

Each role states what starts when the operator comes to want a connection,
and on which thread connecting happens.

**Failure classification.** A failure to set up, bind, listen or connect is
one of:

- **permanent**: waiting cannot fix it: the library rejects a parameter
  value, or the system refuses the operation for lack of permission.
  Validation at construction catches what it can
  ([common.md §1](common.md#1-parameters)). What remains is raised, with an
  error naming the operator and the cause, from the call that made the
  connection wanted (the operator's start, or `connect()`) when it happens
  there; what a failed start does to the process is the clock's handling of
  a failed source. When it happens later, in background connection work, it
  is logged at level 2 and the role stops trying until the connection is
  wanted again.
- **transient**: anything that may clear up later: an address in use or not
  yet available, a name that does not resolve, a peer that is unreachable,
  times out or rejects the handshake. It is logged at level 3 with its cause
  and retried after `polling_delay` seconds while the connection is wanted.

The error a failing library call reports
([library.md §6](library.md#6-what-is-relied-upon-from-the-binding)) decides
the class: a libsrt invalid-parameter or invalid-operation error, or a
system error of permission denied or operation not permitted, is permanent;
every other failure is transient. libsrt reports a bind refused by the
system (port held by another program, privileged port, address not local)
as one generic socket error, so the system error is what tells them apart.

Retrying has no limit and no back-off. Connecting MUST NOT block the
streaming thread, and MUST NOT hold a worker of a shared bounded pool for the
duration of a blocking call ([concurrency.md §1](concurrency.md#1-threads)).

## 5. Connected

When a socket becomes a peer:

1. The kind's own reaction to the new peer runs (input: open the dump,
   start the reader; output: mark the peer as joining the stream).
2. `on_connect` fires.
3. The peer becomes live.

A synchronous `on_connect` therefore completes before any data is exchanged
with that peer. It runs under no operator lock
([concurrency.md §2](concurrency.md#2-callbacks-and-locks)).

## 6. Sockets

- **sockets**: the live peers, as a list of (remote address, socket).
- **connected**: the list is not empty.

## 7. Peer failure

A peer **fails** when an exchange with it fails: a send or receive error,
the library reporting the connection broken or closed by the remote end, or
the stall rules of [output.md §5](output.md#5-sending). The first failure of
a peer is acted upon; later reports about the same peer are ignored.

A failed peer is ended (§8). Only that peer is affected. Each role states
what it does next.

## 8. Disconnect

**Ending one peer**, whatever the cause:

1. It stops being live.
2. Its socket is closed (§3, with the `"close"` event).
3. The kind's own reaction runs (input: close the decoder and the dump;
   output: drop the peer's stream).
4. `on_disconnect` fires.

`on_connect` and `on_disconnect` are paired per peer: each peer that fired
`on_connect` fires `on_disconnect` exactly once, after it, and no other
`on_disconnect` fires. A failed attempt or a refused handshake fires
neither.

**Releasing**, when the connection stops being wanted: every peer is ended,
pending work (an attempt in flight, a scheduled retry, the listening socket)
is abandoned, and its sockets are closed. An attempt that completes after
the release is discarded (§2).

An end the operator initiates (release, stop, shutdown) is **clean** for an
output: each peer receives its pending bytes before its socket closes
([output.md §6](output.md#6-stopping)). A peer failure is not clean.

## 9. Stopping

Process shutdown releases every operator (§8). The release MUST complete
within a bound: no attempt keeps running, no socket stays open, and the
wait for a clean end is limited by `write_timeout`, or by `shutdown_drain`
([output.md §6](output.md#6-stopping)) when it is null.
