# `output.url` at the encoder seam

Part A. Seam view: how `output.url` answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files). Its destination is delegated:
the writer opens the URL and writes to it itself, and no byte crosses the
seam ([encoder.md §6](encoder.md#6-the-delegated-destination),
[sink.md §8](sink.md#8-delegated-destination)). Its states, errors and
parameters are the FFmpeg tree's
([io.md §1.4, §11](../../optionals/ffmpeg/spec/io.md#11-outputurl)). This
file states only what binds the seam, and links the rest.

## 1. Formats and declarations

- **Declarations**: one capability, `delegated(url)`; no reader starts
  mid-stream ([encoder.md §5.1](encoder.md#51-what-an-output-declares)).
- **Check** ([encoder.md §5.3](encoder.md#53-the-check)): a configuration
  that does not accept a delegated destination is refused at evaluation,
  with an invalid-value error on the format argument. `%ffmpeg` is the one
  that accepts it. The output never rewrites the format.
- **Local files are `output.file`'s.** A URL with the `file:` scheme, and a
  string that is not a URL (a plain local path), are refused with an
  invalid-value error on the URL that names `output.file`. The URL is a
  constant string, so the check always runs at evaluation. Reason: `output.file` owns naming, permissions, append and
  finalisation of local files; through a protocol URL each of them is
  silently lost or done twice.
- Standard-library wrappers (`output.youtube.live.rtmp` and others) are
  `output.url` with a URL their own arguments build, a constant string by
  the time `output.url` receives it.

## 2. The sink

None: the writer opens, writes and closes the URL under the output's
lifecycle and interrupt. The output keeps the URL's naming, its restart
policy and its callbacks.

## 3. Writer lifetime

One writer per connection attempt, built on the output's own task, never on
the streaming thread; building it opens the URL. Its initial metadata is
[output-encoded.md §3.2](output-encoded.md#32-initial-metadata)'s. A stop that wins over a build
in progress ends the new writer `abandoned` before its first piece.

## 4. Pieces and metadata

- The streaming work hands each piece to the writer's task through a
  bounded queue, metadata first, and never waits for it
  ([encoder.md §6](encoder.md#6-the-delegated-destination), item 3;
  `delegated_queue_bound` is the output's `timeout`). While the URL is being
  opened, past the bound the oldest pieces are dropped with a warning that
  states the stream time dropped; once open, past the bound is an output
  fault.
- A delegated destination offers no in-band metadata
  ([encoder.md §6](encoder.md#6-the-delegated-destination), item 5), and
  the output has no out-of-band channel: a metadata change is recorded as
  the current metadata and reaches the destination as the initial metadata
  of the next writer.

## 5. Markers and readers

Nothing: a delegated writer writes no marker.

## 6. End

At the output's stop the writer is ended `clean` on its task: it drains and
writes the container's trailer to the URL, bounded by `timeout` through the
interrupt. After a fault it is ended `abandoned`. `on_stop` follows the
output's stop, not the writer's end.

## 7. Reopen, reconnect, append

Each attempt is a new writer: a new connection, its container and
timestamps starting fresh. The wait between attempts is `restart_delay`
([io.md §11.3](../../optionals/ffmpeg/spec/io.md#113-errors)). Append:
nothing.

## 8. Faults and restart policy

The writer reports typed faults ([encoder.md §8](encoder.md#8-faults)); the
classification and the one policy (`restart_delay`, `null` to stop) are
[io.md §11.3](../../optionals/ffmpeg/spec/io.md#113-errors). Every output
fault reaches that policy exactly once, whichever call raised it.

## 9. Concurrency

The writer runs on the output's own task, as work allowed to block
([encoder.md §6](encoder.md#6-the-delegated-destination), item 3): every
call on it, its creation and its end included, runs there, in the order
handed. The streaming work only queues pieces and metadata, and posts
starts and stops. The output's interrupt aborts any blocking open or write
at a stop, at shutdown, or past `timeout`.

## 10. Tests

[io.md §11.3 conformance](../../optionals/ffmpeg/spec/io.md#113-errors).
Seam-binding:

- a format that does not accept a delegated destination is refused at
  evaluation;
- `output.url` given `"file:/tmp/x.ogg"` or `"/tmp/x.ogg"` is refused at
  evaluation, naming `output.file`;
- a URL that accepts slowly: the streaming thread's latency stays bounded,
  another output on the clock keeps its rate, and the destination receives
  the stream from its first piece within `delegated_queue_bound`;
- a destination that stops reading: the output reports a fault within
  `timeout` and restarts after `restart_delay`;
- a clean stop of a connected output writes the container's trailer.
