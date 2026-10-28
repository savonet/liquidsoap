# Dedicated encoder mode

`dedicated_encoder=true`. Each listener gets its own encoder, created when it
connects, so every listener receives a stream that starts from an encoder's
first byte. It serves formats a listener cannot join mid-stream. There is no
shared store and no burst.

Numbered as [modes.md](modes.md).

**Output-wide state**: the encoder factory, set while started.

**Cost.** Every frame piece is encoded once per listener, on the streaming
clock, so the cycle's cost grows with the audience. This mode suits a small
number of listeners. An implementation MAY spread the per-listener encoding
of one piece over parallel work, provided each listener's encoder receives
its pieces in order and the cycle's send completes only when every listener
has its piece.

## 1. Listener state

- its own encoder, created from the factory during admission, named with the
  output's id, with the last metadata as initial metadata;
- a queue of bytes owed;
- the length of the stream bytes owed that precede the newest encoder output,
  for the overflow rule.

Every use of a listener's encoder is serialised: encoding, in-stream
metadata, and its stop never overlap.

## 2. Initial bytes

The encoder's header, with ICY blocks inserted
([icy.md §4](icy.md#4-cadence-and-change-detection)) stamped with the last
metadata, queued after the response head.

## 3. Pending test

The queue is non-empty.

## 4. Flush

Offer the queue's front in one write and drop from it what was accepted. On
−1, mark the listener closed. Appends from send and drops by flush MUST NOT
lose or reorder bytes when they run at the same time.

## 5. Encode and send

_Encode_ keeps the frame piece and returns nothing.

_Send_, when a piece is kept, with `M` the last metadata:

1. For each listener published and not marked closed: encode the piece with
   its encoder, insert ICY blocks into the result stamped with `M`, append
   the result to its queue, then check overflow
   ([modes.md §4](modes.md#4-flush)) and disconnect it if it is over.
2. Wake the groups.

Encoding MUST NOT use an encoder that is being stopped: the closed flag is set
before teardown can run, and the check of the flag and the use of the
encoder are serialised with the stop.

Only published listeners get pieces, so every queue starts with its response
head and header. A listener published after send has listed the listeners
does not get this piece; its stream starts with its header and continues with the next piece.

## 6. Metadata

Record the event, then pass the converted values, with cover art removed, to
every listener's encoder as in-stream metadata, serialised with its encoding.
The in-stream metadata a listener receives is the same as in shared mode.

## 7. Start

Store the encoder factory.

## 8. Stop

Clear the factory. Stop every listener's encoder and queue the bytes it
returns ([§11](#11-final-bytes)).

## 9. Listener teardown

Stop the listener's encoder unless the output's stop already did, and
discard what it returns: the socket is closed by then.

## 10. Staging

The queue is offered in place; nothing to stage ([modes.md §10](modes.md#10-staging)).

## 11. Final bytes

Each listener's encoder's final bytes are appended to that listener's queue
on stop.
