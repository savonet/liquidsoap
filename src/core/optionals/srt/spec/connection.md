# The connection-role contract

A role owns the SRT sockets of one operator: how they are created, connected,
listed, lost and closed. The operator kind ([input.md](input.md),
[output.md](output.md)) only asks for the operations below and listens to the
events. [caller.md](caller.md) and [listener.md](listener.md) answer each
section under the same number.

## 1. Parameters

Which of [common.md §1](common.md#1-parameters) the role reads.

## 2. State

What the role holds between calls.

## 3. Socket setup

Every socket a role creates goes through these steps, in order:

1. Create a libsrt socket.
2. Set payload size to `payload_size`.
3. Set transport type to live.
4. Set the message API flag to `messageapi`.
5. Set enforced encryption to `settings.srt.enforced_encryption`.
6. Fire `on_socket` with the creation mode (`"connect"` or `"listen"`).

The role then sets its own options. Those come after `on_socket`, so a script
setting one of them in `on_socket` is overridden.

**Closing** a socket the role holds: fire `on_socket` with `"close"`, then
close it. Sockets that fail during setup are closed without the event.

## 4. Connect

What `connect` starts, and on which thread the connection is made.

## 5. Connected

When `on_connect` fires, on which thread, under which lock, and whether the
new socket is already listed.

## 6. Sockets

- **sockets**: the list of (peer address, socket) the operator exchanges data
  with.
- **connected**: the list is not empty.

## 7. Peer failure

What the role does when the output fails to send to one socket
([output.md §5](output.md#5-chunking-and-sending)). The input never reports a
peer failure; it disconnects ([input.md §5](input.md#5-producing-frames)).

## 8. Disconnect

What `disconnect` closes, whether `on_disconnect` fires, and what it leaves
for a later `connect`.

## 9. Stopping

How the role reads the stop flag ([common.md §5](common.md#5-stop-flag)),
and what process shutdown does to it.
