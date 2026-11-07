# The caller role

Connects to a remote listener and keeps at most one peer. Numbered as
[connection.md](connection.md).

## 1. Parameters

`host`, `port`, `prefer_address`, `streamid`, `passphrase`, `pbkeylen`,
`enforced_encryption`, `polling_delay`, `connection_timeout`,
`payload_size`, `messageapi`, and the kind's timeout (`read_timeout` for the
input, `write_timeout` for the output).

## 2. State

One of:

- **idle**: the connection is not wanted;
- **attempting**: one attempt (§4) is in flight;
- **waiting**: the next attempt is due at a known time;
- **connected**: one peer.

Each attempt carries the identity of the transition that started it. An
attempt whose identity is no longer current when it completes is discarded
([connection.md §2](connection.md#2-state)): this is what makes a
`disconnect()` during an attempt leave the operator idle, with no socket and
no event.

## 3. Socket setup

After [connection.md §3](connection.md#3-socket-setup) steps 1–5, the
caller's own options, each only when given:

1. `streamid`;
2. `enforced_encryption`;
3. `pbkeylen`;
4. `passphrase` ([common.md §2](common.md#2-encryption));
5. connect timeout from `connection_timeout`.

Then `on_socket` fires with `"connect"`.

## 4. Connect

When the connection becomes wanted, an attempt starts at once.

**Attempt:**

1. Resolve `host`:`port` ([library.md §5](library.md#5-address-resolution)).
   Log at level 4 `Connecting to srt://<host>:<port>`.
2. For each resolved address in the resolver's order, until one connects:
   create and set up a socket (§3) and connect it, waiting up to the connect
   timeout. A socket that fails to connect is closed (with its `"close"`
   event).
3. On success, if the attempt is still current: log at level 3
   `Connected to srt://<ip>:<port>`, and the socket becomes the peer
   ([connection.md §5](connection.md#5-connected)).

A failure is classified by
[connection.md §4](connection.md#4-connect): a transient one logs at level 3
`Connection to srt://<host>:<port> failed: <cause>`, with the library's
rejection reason when the peer refused the handshake, and moves to waiting
for `polling_delay` seconds.

`connect()` while attempting, waiting or connected does nothing: there is
never more than one attempt or one peer. Attempts MUST NOT occupy a worker
of a shared bounded pool while they block
([concurrency.md §1](concurrency.md#1-threads)).

## 5. Connected

`on_connect` fires on the thread that completed the attempt, after the
kind's own reaction and before the peer is live
([connection.md §5](connection.md#5-connected)).

## 6. Sockets

The peer, as a one-element list, or the empty list. The address is the
remote address the attempt connected to.

## 7. Peer failure

The peer is ended ([connection.md §8](connection.md#8-disconnect)). If the
connection is still wanted, a new attempt starts at once when the peer was
live for at least `polling_delay`, and after `polling_delay` otherwise: a
peer that fails as soon as it connects (a decoder that cannot be found, a
peer that drops every stream) never makes the caller reconnect without
pause.

## 8. Disconnect

Releasing ([connection.md §8](connection.md#8-disconnect)) from each state:

- **attempting**: the attempt is abandoned: its socket is closed, so that a
  connect blocked on it returns, and whatever the attempt later produces is
  discarded; the attempt fires the socket's `"close"` event;
- **waiting**: the scheduled attempt is cancelled;
- **connected**: the peer is ended.

The role is then idle.

## 9. Stopping

At process shutdown the role is released (§8). An attempt blocked in the
library is not waited for: releasing closes its socket, which makes the
connect return at once, and the attempt then fires the socket's `"close"`
event and discards its result (§2).
