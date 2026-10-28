# Dedicated encoder mode

`dedicated_encoder=true`. Each listener gets its own encoder, created when it
connects, so every listener receives a stream that starts from the encoder's
first byte. There is no ring and no burst.

Numbered as [modes.md](modes.md).

**Output-wide state**: the encoder factory, set while started; the current
frame piece, set by encode and cleared by send.

## 1. Listener state

- its own encoder, created from the factory on the request task, named with
  the output's id, with empty initial metadata;
- a pending byte queue;
- an encoder lock, held around every use of this encoder.

Refused with 404 when the output is not started.

## 2. Initial bytes

The encoder's header, with ICY blocks inserted (as in
[shared-encoder.md §2](shared-encoder.md#2-initial-bytes) step 3), appended to
the pending queue after the response head. This append is not checked against
`buffer`.

## 3. Pending test

The pending queue is non-empty.

## 4. Flush

Under the queue's lock, offer the whole queue to the socket in one write; drop
from its front what was accepted. On −1, mark the listener closed. Overflow is
not checked here ([§5](#5-encode-and-send)).

## 5. Encode and send

_Encode_ stores the frame piece and returns nothing.

_Send_, when a piece is stored:

1. Read the output's last metadata once.
2. For each listener currently listed (published, not marked closed), one
   after the other on the streaming thread:
   1. if it is marked closed, skip it;
   2. take its encoder lock; check again that it is not marked closed;
   3. encode the piece with its encoder; insert ICY blocks into the result
      with the metadata of step 1 (a block goes wherever the position reaches
      `metaint`, including at the very end of the result);
   4. if the queue length plus the result's length exceeds `buffer`, append
      nothing; otherwise append the result;
   5. release the lock; if nothing was appended, disconnect for overflow.
3. Clear the stored piece and wake the shards.

The double check of the closed flag around the lock excludes encoding into an
encoder that teardown ([§9](#9-listener-teardown)) is stopping: the flag is
set before teardown is queued, and teardown takes the same lock. The overflow
disconnection happens after releasing the lock for the same reason.

Listeners admitted but not yet published when send runs do not get this piece;
their stream starts with their header and continues with the next piece.

The ICY block sent for a byte reflects the last metadata at the time the byte
was encoded.

## 6. Metadata

Convert and store as last metadata. Nothing is passed to the per-listener
encoders: they never receive in-stream metadata.

## 7. Start

Store the encoder factory.

## 8. Stop

Clear the factory. Per-listener encoders are stopped by each listener's
teardown once the stop marks it closed.

## 9. Listener teardown

Under the listener's encoder lock, call its encoder's stop and discard the
bytes it returns.

## 10. Staging size

0: the staging buffer is unused.

## 11. Dump file

Nothing. The file is created (truncated) on start and closed on stop, and stays
empty.
