# The encoder-mode contract

The operator has two modes, chosen by `dedicated_encoder`. Everything else
(routing, admission, delivery groups, disconnection, callbacks) is common. A
mode supplies the clauses below. [shared-encoder.md](shared-encoder.md) and
[dedicated-encoder.md](dedicated-encoder.md) MUST answer each clause under
the same number; a clause a mode does not need is answered "nothing", never
left out.

The generic output base drives the streaming side: for each frame, it cuts
the frame at its metadata positions and, for each piece, calls _metadata_
([§6](#6-metadata)) when a metadata falls inside the piece, then _encode_,
then _send_ ([§5](#5-encode-and-send))
([contracts.md §3](contracts.md#3-output-base)).

Both modes offer the same features, as far as their structure allows: ICY
stamped at encode time, in-stream metadata in the encoded stream, the
overflow rule of [§4](#4-flush), the final bytes of [§11](#11-final-bytes).
Only the burst is specific to the shared mode, because a dedicated encoder
has no history to send.

## 1. Listener state

What the mode keeps per listener besides the common fields (record, socket,
closed flag, ICY position and last generation sent, last progress). Created
during admission ([connection.md §6](connection.md#6-admission), step 5).
When the mode's encoder state is not running, creation refuses the request
with the 503 reply of that step.

## 2. Initial bytes

What a new listener is owed before live data, queued during admission behind
the response head ([connection.md §6](connection.md#6-admission), step 7). It
runs before the listener is published, so nothing else touches the listener
yet. Initial bytes do not count towards `buffer` ([§4](#4-flush)).

A listener admitted with a preroll owes it right after the response head,
before the mode's initial bytes. Its live data starts only once the preroll
has been accepted by its socket, as Icecast's intros: a preroll longer than
`buffer`, read by a slow client, never leads to overflow. The preroll's bytes
are stream data for ICY ([icy.md §4](icy.md#4-cadence-and-change-detection)).
A preroll MUST NOT be copied per listener: the same ad commonly goes to every
listener.

## 3. Pending test

Whether the listener is owed bytes. Delivery flushes and times out only
listeners for which this holds ([delivery.md §3](delivery.md#3-the-delivery-pass)).

## 4. Flush

Write what the listener is owed, with at most one socket write
([delivery.md §4](delivery.md#4-writing-to-a-socket)), and account for what
the socket accepted. Mark the listener closed on a write error or on
overflow. Runs only in a delivery pass of the listener's group.

**Overflow.** A listener is in overflow when the encoded stream bytes it is
owed that precede the newest encoder output exceed `buffer`. Only stream
bytes count: the response head, the codec header and ICY blocks do not. The
newest encoder output does not count, so a single output larger than
`buffer` (a video keyframe) does not disconnect a listener that was caught up
before it. The rule is the same in both modes; it bounds how far behind live
a listener can be, and with it the memory and retention that listener costs.

## 5. Encode and send

_Encode_ receives a piece of frame; _send_ receives what encode returned. Both
run on the streaming clock. Send ends by waking every group that has members
([delivery.md §2](delivery.md#2-wake-ups)). The ICY stamping of the bytes
produced is fixed here: each byte is in the scope of the last metadata at
the time it was encoded ([icy.md §4](icy.md#4-cadence-and-change-detection)).

## 6. Metadata

Called on the streaming clock with each metadata of the source. Every mode
converts it ([icy.md §5](icy.md#5-charset)), records it as a new event and the
output's last metadata, and passes the converted values, with cover art
removed, to every running encoder as in-stream metadata.

## 7. Start

Create the mode's encoder state. Runs under the output's start/stop
serialisation, before the delivery groups and the registration exist
([operator.md §7](operator.md#7-lifecycle)). Encoders are created with the
last metadata as initial metadata, empty when there is none.

## 8. Stop

Stop the mode's encoder state. Runs under the same serialisation, after the
registration is removed and before the last delivery pass. From then on
admissions are refused with 503 and encode produces nothing.

## 9. Listener teardown

Release the per-listener state. Runs once per listener, in its teardown,
before the `on_disconnect` handlers ([delivery.md §6](delivery.md#6-disconnection)).

## 10. Staging

How a flush assembles the bytes it offers in one write. It MAY copy them into
a staging area or offer them in place; this is not observable and is the
implementation's choice.

## 11. Final bytes

Where the bytes an encoder returns when it is stopped go. They MUST be queued
for the listeners that encoder served, behind everything already owed, so
that the last delivery pass of [delivery.md §7](delivery.md#7-stopping) can
send them (for Ogg, the end-of-stream pages). A listener whose socket has no
room for them is closed without them.
