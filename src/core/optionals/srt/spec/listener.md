# The listener role

Listens on a local address and keeps a list of accepted peers. Numbered as
[connection.md](connection.md).

## 1. Parameters

`bind_address`, `port`, `prefer_address`, `ipv6only`, `listen_callback`,
`passphrase`, `pbkeylen`, `enforced_encryption`, `polling_delay`,
`payload_size`, `messageapi`, the kind's timeout (`read_timeout` for the
input, `write_timeout` for the output), and a **client limit**: the output's
`max_clients` (null: no limit), 1 for the input.

## 2. State

One of:

- **idle**: the connection is not wanted;
- **waiting**: setting up the listening socket failed transiently; the next
  try is due at a known time;
- **listening**: one listening socket, and the peers accepted through it.

A peer is **admitted** from the moment its accept lands until it is ended;
live peers ([connection.md §2](connection.md#2-state)) are the admitted ones
whose `on_connect` has completed. The client limit counts admitted peers.

The listening socket lives exactly as long as the connection is wanted:
losing a peer never touches it.

## 3. Socket setup

**Listening socket.** After [connection.md §3](connection.md#3-socket-setup)
steps 1–5:

1. Resolve `bind_address`:`port` ([library.md §5](library.md#5-address-resolution)).
   The IPv4 wildcard `0.0.0.0` means any address: under the `ipv6`
   preference it resolves as the IPv6 wildcard `::`, so that the default
   listener exists under every preference.
2. IPv6-only flag: `ipv6only` if given; otherwise `true` when the resolved
   address is IPv6; otherwise left as is.
3. `enforced_encryption`, `pbkeylen`, `passphrase`, each when given
   ([common.md §2](common.md#2-encryption)).
4. Fire `on_socket` with `"listen"`.
5. Bind to the resolved address.
6. Install the admission predicate (below).
7. Listen. The backlog SHOULD be at least the client limit when there is
   one.
8. Log at level 4 `Listening on srt://<ip>:<port>`.

**Admission predicate**, consulted by the library during each handshake:

1. With a client limit `n`: reject when `n` peers are admitted.
2. With `listen_callback`: call it with `hs_version`, `peeraddr` (the peer's
   IP without port), `streamid` (null when the caller sent none) and the new
   socket ([socket-value.md](socket-value.md)); admit on `true`.
3. Otherwise admit.

An exception from `listen_callback` rejects the handshake and is logged at
level 2. The predicate runs on the library's handshake thread and holds no
operator lock; a slow `listen_callback` delays every handshake of the
process, which its documentation states.

The predicate's count is advisory, since a handshake it admits completes
later. The limit is exact at accept (§4).

**Accepted socket.** After [connection.md §3](connection.md#3-socket-setup)
steps 6–7: the options the kind needs for exchanging data, then `on_socket`
with `"incoming"`. Steps 1–5 do not apply: an accepted socket inherits the
listening socket's options.

## 4. Connect

When the connection becomes wanted: set up the listening socket (§3) and
register it with the readiness loop for incoming connections
([poll.md §2](poll.md#2-registering-and-removing)). The first setup runs in
the call that made the connection wanted, so a permanent failure is raised
from it ([connection.md §4](connection.md#4-connect)); a transient one logs at
level 3 `Listening on srt://<ip>:<port> failed: <cause>` and moves to
waiting for `polling_delay` seconds.

The listening socket is registered after it listens. A handshake that
completes in between makes it ready before it is registered, which the
readiness loop does not report: right after registering, the role accepts
once, as if the socket had reported a connection.

**Accepting**, each time the listening socket reports a connection, until
no connection is pending:

1. Accept: (socket, origin).
2. Log at level 4 `New connection from <ip>:<port>`.
3. In one atomic step: if the connection is no longer wanted, or `n` peers
   are already admitted, discard the socket (closed, no event); otherwise
   admit it.
4. Set it up as an accepted socket (§3).
5. Connected ([connection.md §5](connection.md#5-connected)).

An accept that fails is logged at level 3 and the listening socket stays
registered: a failure on one connection never stops the listener from
accepting the next. Any failure in steps 4–5 ends that peer alone.

## 5. Connected

`on_connect` fires once per admitted peer, after the kind's own reaction
and before the peer is live. It never runs on the readiness loop's thread
([concurrency.md §1](concurrency.md#1-threads)): a slow `on_connect` delays
the events of its own operator, which run one at a time
([concurrency.md §2](concurrency.md#2-callbacks-and-locks)), while the
listener keeps accepting and live peers keep exchanging data.

## 6. Sockets

The live peers, newest first. The address is the peer's origin.

## 7. Peer failure

The peer is ended ([connection.md §8](connection.md#8-disconnect)). The
listening socket and every other peer are untouched, and a new peer may be
admitted in its place.

## 8. Disconnect

Releasing ([connection.md §8](connection.md#8-disconnect)):

1. Remove the listening socket from the readiness loop, close it (with its
   `"close"` event). New handshakes are refused from here on.
2. End every admitted peer; for an output, cleanly.
3. From waiting, cancel the scheduled try.

The role is then idle.

## 9. Stopping

At process shutdown the role is released (§8).
