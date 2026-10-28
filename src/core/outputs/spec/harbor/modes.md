# The encoder-mode contract

The operator has two modes, chosen by `dedicated_encoder`. Everything else —
routing, admission, shards, writers, disconnection, callbacks — is common.
A mode supplies the clauses below. [shared-encoder.md](shared-encoder.md) and
[dedicated-encoder.md](dedicated-encoder.md) answer each clause under the same
number.

The generic output base drives the streaming side: for each frame, it cuts
the frame at its metadata positions and, for each piece, calls _metadata_
([§6](#6-metadata)) when a metadata falls inside the piece, then _encode_,
then _send_ ([§5](#5-encode-and-send)) — metadata first
([contracts.md §3](contracts.md#3-output-base)).

## 1. Listener state

What the mode keeps per listener besides the common fields (record, socket,
closed flag, ICY position and last metadata sent, last progress). Created on
the request task during admission
([connection.md §5](connection.md#5-admission), step 4). When the mode's
encoder state is not running, creation refuses the request with
`HTTP/<protocol> 404 Not found\r\n`.

## 2. Initial bytes

What a new listener is owed before live data, queued during admission with the
response head in front ([connection.md §5](connection.md#5-admission),
step 7). It runs before the listener is published, so nothing else touches the
listener yet.

## 3. Pending test

Whether the listener is owed bytes. Writers flush and time out only listeners
for which this holds ([delivery.md §3](delivery.md#3-the-writer-pass)).

## 4. Flush

Write what the listener is owed, with at most one socket write
([delivery.md §4](delivery.md#4-writing-to-a-socket)), and account for what
the socket accepted. Mark the listener closed on a write error or when it is
more than `buffer` bytes behind. Runs only on the writer of the listener's
shard.

## 5. Encode and send

_Encode_ receives a piece of frame; _send_ receives what encode returned. Both
run on the streaming thread. Send ends by waking every shard that has members
([delivery.md §2](delivery.md#2-wake-ups)).

## 6. Metadata

Called on the streaming thread with each metadata of the source. Every mode
converts it ([icy.md §5](icy.md#5-charset)) and stores it as the output's last
metadata ([icy.md §4](icy.md#4-cadence-and-change-detection)).

## 7. Start

Create the mode's encoder state. Runs under the output's start/stop lock,
before the shards and the HTTP handler exist
([operator.md §7](operator.md#7-lifecycle)).

## 8. Stop

Tear down the mode's encoder state. Runs under the same lock, before the
handler is removed and listeners are marked closed.

## 9. Listener teardown

Release the per-listener state. Runs once per listener, on a `Threaded`
scheduler task, before the `on_disconnect` handlers
([delivery.md §6](delivery.md#6-disconnection)).

## 10. Staging size

The size of each shard's staging buffer, fixed when the shards are created.

## 11. Dump file

What is written to `dumpfile` while it is open.
