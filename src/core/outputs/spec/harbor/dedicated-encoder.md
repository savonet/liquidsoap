# Dedicated encoder mode

`dedicated_encoder=true`. Each listener gets its own writer and its own
sink, so every listener receives a stream that starts from a writer's first
byte, which begins with the header. It serves configurations a listener
cannot start mid-stream. There is no shared stream and no burst.

Numbered as [modes.md](modes.md).

**Output-wide state**: the writer factory, set while started.

**Cost.** Every piece is encoded once per listener, by the output's
streaming work, so the cycle's cost grows with the audience. This mode suits
a small number of listeners. An implementation MAY spread the per-listener
encoding of one piece over parallel work, provided each listener's writer
receives its pieces in order, from one task at a time, and the cycle's send
completes only when every listener has its piece.

## 1. Listener state

- its own writer, built by the streaming work before the listener's first
  piece ([§5](#5-encode-and-send)), named with the output's id, with its
  initial metadata
  ([output-encoded.md §3.2](../../../encoder/spec/output-encoded.md#32-initial-metadata));
  an admission only posts the listener, and never builds or calls a writer;
- its sink: a queue of bytes owed, which ignores every marker but `end`;
- the length of the stream bytes owed that precede the newest writer call's
  bytes, for the overflow rule;
- the length of the preroll still owed.

Queued bytes are slices of what they come from. The head, the preroll and
ICY blocks are strings, so a preroll shared by many listeners is not copied
per listener. The bytes a writer lends its sink
([sink.md §2](../../../encoder/spec/sink.md#2-operations)) are copied into
the listener's chunks: fixed buffers it reuses, each returned to its pool
once no queued slice refers to it, so a listener caught up with the stream
keeps reusing one. The pool keeps a few chunks; the others are released.

Only the output's streaming work calls a listener's writer
([encoder.md §7](../../../encoder/spec/encoder.md#7-concurrency)).

## 2. Initial bytes

The preroll, if any, with empty ICY blocks inserted, queued after the
response head. The listener's stream follows: its writer's first stream
start, header first, with ICY blocks inserted as into any stream data
([icy.md §4](icy.md#4-cadence-and-change-detection)).

## 3. Pending test

The queue is non-empty.

## 4. Flush

Offer the front of the queue in one write and drop from it what was accepted.
A large slice of a chunk at the front is offered from its chunk; otherwise the
front is offered through the staging area, as much of it as that holds
([§10](#10-staging)). On −1,
mark the listener closed. Appends from send and drops by flush MUST NOT lose
or reorder bytes when they run at the same time.

## 5. Encode and send

_Encode_ keeps the piece.

_Send_, when a piece is kept, with `M` the last metadata:

1. For each listener published, not marked closed, and not owing any of its
   preroll: build its writer if it has none, hand it the piece (`write`),
   insert ICY blocks stamped with `M` into the bytes its sink received,
   append them to its queue, then check overflow
   ([modes.md §4](modes.md#4-flush)) and disconnect it if it is over.
2. End `abandoned` the writer of every listener marked closed since the
   last send.
3. Wake the groups.

Only published listeners get pieces, so every queue starts with its response
head. A listener published after send has listed the listeners does not get
this piece; its stream starts with its writer's header at the next piece.

An output fault of a listener's writer is that listener's fault: the
listener is closed, the others go on
([output-harbor.md §8](../../../encoder/spec/output-harbor.md#8-faults-and-restart-policy)).

## 6. Metadata

Record the event, then offer the converted values, with cover art removed,
to every listener's writer under the gate of
[modes.md §6](modes.md#6-metadata). The in-band metadata a listener receives
is the same as in shared mode.

## 7. Start

Store the writer factory.

## 8. Stop

Clear the factory. End every listener's writer clean and queue its final
bytes ([§11](#11-final-bytes)).

## 9. Listener teardown

Release the queue. The teardown never calls the writer: a writer still
running is ended `abandoned` by the streaming work
([§5](#5-encode-and-send), step 2), and what it wrote is discarded.

## 10. Staging

The front of the queue is copied into a staging area and offered in one
write ([modes.md §10](modes.md#10-staging)). The queue holds many small
pieces (an empty ICY block is one byte), so offering one piece per write
would multiply the writes.

## 11. Final bytes

Each listener's writer's final bytes are appended to that listener's queue
on stop.
