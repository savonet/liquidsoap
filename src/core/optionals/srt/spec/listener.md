# The listener role

Listens on a local address and keeps a list of accepted peers. Numbered as
[connection.md](connection.md).

## 1. Parameters

`bind_address`, `port`, `prefer_address`, `ipv6only`, `listen_callback`,
`passphrase`, `pbkeylen`, `enforced_encryption`, `read_timeout`,
`write_timeout`, `payload_size`, `messageapi`, and a client limit: the
output's `max_clients` (null: no limit), fixed at 1 for the input.

## 2. State

- **listening socket**: absent, or one socket, created on demand (§3).
- **clients**: a list of (peer address, socket), newest first.

The operator is entered in the shutdown registry at construction
([library.md §4](library.md#4-shutdown)).

## 3. Socket setup

The listening socket is created the first time connect needs it and kept
until a stopping disconnect (§8). After
[connection.md §3](connection.md#3-socket-setup) with mode `"listen"`, in
this order:

1. Resolve `bind_address`:`port` ([library.md §5](library.md#5-address-resolution)).
2. IPv6-only flag: set to `ipv6only` if given; otherwise set to `true` if the
   resolved address is IPv6; otherwise leave it.
3. Bind to the resolved address.
4. Build the **admission predicate** and install it if it exists:
   - with a client limit `n`: admit while the clients list is shorter than
     `n`, read under the source lock;
   - with `listen_callback`: call it with `hs_version`, `peeraddr` (the peer's
     IP without port), `streamid` (null if the caller sent none) and the new
     socket without methods, and admit on `true`;
   - with both: the limit is checked first, then the callback, and both must
     admit.
     The predicate runs on a libsrt thread during the handshake
     ([library.md §6](library.md#6-what-is-relied-upon-from-the-binding)); an
     exception rejects.
5. `enforced_encryption`, `pbkeylen`, `passphrase`, each if given.
6. Listen, with a backlog of the client limit, or 1 when there is none.
7. Log at level 4 `Setting up socket to listen at <ip>:<port>`.

Any failure closes the socket without the `close` event and is raised to the
caller of connect. Accepted sockets are configured in §4.

## 4. Connect

If the operator is stopping, nothing. Otherwise, under the source lock: get
or create the listening socket (§3), and register it with the readiness loop
for read readiness with the accept handler ([poll.md §2](poll.md#2-registering-and-removing)).

The **accept handler** runs on the readiness loop's thread:

1. Accept a connection from the listening socket: (client, origin).
2. Log at level 4 `New connection from <ip>:<port>`; if formatting the
   address fails, log at level 3 `Error while fetching connection source: <exn>`
   and go on.
3. Register the listening socket again, for the next connection.
4. Set on the client: blocking send, blocking receive, send timeout from
   `write_timeout` if given, receive timeout from `read_timeout` if given.
5. Fire `on_socket` with `"incoming"`.
6. If the operator is stopping: close the client (with the `close` event)
   and give up.
7. Prepend (origin, client) to the clients, under the source lock.
8. Fire `on_connect` (§5).

Any exception in steps 3–8 closes the client without the event and gives up;
giving up logs at level 5 `Failed to connect: <exn>`. In step 6 this closes
the client a second time; if that second close raises, its error is the one
logged.

An exception in step 1 or 2 is logged the same way, and step 3 never runs:
the listening socket is no longer registered, and no further connection is
accepted until connect runs again.

## 5. Connected

`on_connect` fires once per accepted client, on the readiness loop's thread,
after the client is listed, inside the source lock in its mutating mode
([concurrency.md §2](concurrency.md#2-the-source-lock)).

## 6. Sockets

The clients list, read under the source lock, newest first. The address is
the peer's origin address.

## 7. Peer failure

1. Log the exception with its backtrace as
   `Error while sending client data: <exn>`.
2. Under the source lock in mutating mode: close that client (with the
   `close` event) and remove it from the clients.

`on_disconnect` does not fire. The listening socket is untouched.

## 8. Disconnect

1. Note whether the operator is stopping.
2. Under the source lock in mutating mode:
   1. close every client (with the `close` event), and empty the list;
   2. if stopping and there is a listening socket: remove it from the
      readiness loop, close it (with the `close` event), forget it;
   3. fire `on_disconnect`.

`on_disconnect` fires on every disconnect, whether or not a client was
connected. A non-stopping disconnect keeps the listening socket and its
registration, so new clients are still accepted.

## 9. Stopping

Connect and the accept handler read the operator's stop flag. At process
shutdown the stop flag reads as set, so the registry's disconnect (§8) also
closes the listening socket.
