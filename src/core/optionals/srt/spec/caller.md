# The caller role

Connects to a remote listener and keeps at most one connection. Numbered as
[connection.md](connection.md).

## 1. Parameters

`host`, `port`, `prefer_address`, `streamid`, `passphrase`, `pbkeylen`,
`enforced_encryption`, `polling_delay`, `connection_timeout`, `read_timeout`,
`write_timeout`, `payload_size`, `messageapi`.

## 2. State

- **socket**: absent, or one (resolved remote address, socket) pair.
- **connect task**: absent until the first connect; a task on the scheduler's
  threaded queue running the attempt of §4.
- **task stop flag**: separate from the operator's stop flag; set by
  disconnect, cleared by connect.

The operator is entered in the shutdown registry at construction
([library.md §4](library.md#4-shutdown)).

## 3. Socket setup

After [connection.md §3](connection.md#3-socket-setup) with mode `"connect"`,
in this order:

1. blocking send, blocking receive;
2. `streamid`, if given;
3. `enforced_encryption`, if given;
4. `pbkeylen`, if given;
5. `passphrase`, if given;
6. connect timeout from `connection_timeout`, if given;
7. send timeout from `write_timeout`, if given;
8. receive timeout from `read_timeout`, if given.

## 4. Connect

1. Clear the task stop flag.
2. If there is no connect task, create one and wake it; otherwise wake the
   existing one.

Each run of the task is one **attempt**:

1. Resolve `host`:`port` ([library.md §5](library.md#5-address-resolution)).
2. Log at level 3 `Connecting to srt://<host>:<port>..`.
3. If a socket is held, take it out and close it
   ([connection.md §3](connection.md#3-socket-setup)). `on_disconnect` does
   not fire.
4. Create and set up a socket (§3).
5. Connect it to the resolved address. The call blocks up to the connect
   timeout.
6. Log at level 3 `Client connected!`.
7. Fire `on_connect` (§5).
8. Store (address, socket) as the socket.
9. Sleep until woken.

An exception in steps 5–8 closes the new socket without the `close` event and
fails the attempt. A failed attempt, from any step, logs at level 3
`Connect failed: <exn>` and runs again after `polling_delay` seconds unless
the task stop flag is set, in which case the task sleeps.

Retrying has no limit and no back-off.

Waking the task while a socket is held runs a full attempt: step 3 closes the
live connection and a new one replaces it.

## 5. Connected

`on_connect` fires on the connect task's thread, before the socket is stored:
while it runs the operator is not connected and `sockets()` is empty. No lock
is held.

## 6. Sockets

The held socket, as a one-element list, or the empty list. The address is the
resolved remote address.

## 7. Peer failure

1. Log the exception with its backtrace as
   `Error while sending client data: <exn>`.
2. Disconnect (§8).
3. Unless stopping, connect (§4): a new connect task is created.

## 8. Disconnect

1. Take the socket out. If there was one: close it, then fire
   `on_disconnect`.
2. Set the task stop flag.
3. If there is a connect task: stop it and forget it. Stopping does not
   interrupt an attempt in progress; that attempt runs to its end and may
   store a socket.

`on_disconnect` fires on the thread that called disconnect.

## 9. Stopping

The attempt reads only the task stop flag, never the operator's stop flag.
The operator's stop flag matters in §7 only. At process shutdown the registry
calls disconnect (§8).
