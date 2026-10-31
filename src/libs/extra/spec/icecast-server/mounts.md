# Mounts: lifecycle, feeders, fallbacks and recording

A mount is a path that listeners read. It exists while a feeder serves it or
while it carries another feeder's stream as a fallback. A **feeder** is what
produces a mount's stream: a source client ([sources.md](sources.md)) or a
Liquidsoap source ([§2](#2-what-feeds-a-mount)). Listener admission is in
[listeners.md](listeners.md).

Per-mount configuration comes from the mount callback: given a path, it returns
the **mount parameters**, and a path for which it returns none has the
defaults ([parameters.md](parameters.md)). A mount's own value wins over the
default. The `compatibility` parameter (`"icecast-2.5"` default,
`"icecast-2.4"`, `"rsas"`, `"kh"`) chooses the implementation to mimic where
they differ; rules that depend on it say what each value does.

## 1. Lifecycle

- A mount is created when its first feeder connects or is attached, or when a
  listener asks for a configured mount that has no output while its fallback
  has something to send (below).
- While it exists its feeder may change: the feeder leaves and returns, or the
  mount switches to a fallback ([§6](#6-fallback)). Listeners stay connected
  throughout.
- A mount output that has nothing to send MUST be shut down: its listeners are
  disconnected and its path answers `404`. A mount has something to send while
  a feeder is attached or a fallback in its chain has something to send
  ([§6](#6-fallback)). An attached Liquidsoap source is a feeder for as long as
  it is attached, even while it momentarily has no data ([§2](#2-what-feeds-a-mount)).
  A feeder that returns later, after any `linger` ([§1.1](#11-linger)), creates
  a new mount. Having a fallback configured
  is not enough: the fallback must have something to send. Removing the
  fallback of a mount whose feeder is gone, by reload or by an admin request, is
  one way to reach this state.
- A mount's removal is reported as any disconnection ([sources.md
  §8](sources.md#8-lifecycle), [sources.md §7](sources.md#7-notifications)).
- On server shutdown with feeders connected, mounts end without hanging.

### 1.1 Linger

`linger` (seconds, per mount, [parameters.md](parameters.md#41-fields)) is a
grace period for a feeder that drops. The default is no linger.

- The server SHOULD, when a mount's feeder leaves and nothing else feeds the
  mount (no fallback has something to send), keep the mount output for `linger`
  seconds with its listeners attached, sending nothing. `linger` is read when
  the feeder leaves. The server asks the harbor output for this once, when it
  creates the output: the output keeps its listeners while its source is
  unavailable ([harbor icecast-server.md §2](../../../../core/outputs/spec/harbor/icecast-server.md#2-what-the-server-asks-of-the-output)),
  and the server alone decides when to shut the output down.
- A source client or Liquidsoap source that takes the mount within the window
  feeds it, and the stream continues at a packet boundary. If it has another
  format the output is replaced as in [§3](#3-the-format-match-rule).
- If nobody arrives, the shutdown rule above applies at the end of the window.
  A fallback that gets something to send during the window takes the mount at
  once.
- A source-disconnect event fires when the feeder leaves, not at the end of the
  window.

**Improvement.** The server reads the stream, so listeners can wait at a packet
boundary and a source blip is invisible to them; Icecast disconnects them with
the source. This is not subject to `compatibility`. Icecast-KH's `linger-for`
translates onto `linger` ([icecast-config.md](icecast-config.md#5-mount)).

**A configured mount with no output.** A mount that is only another mount's
fallback, with no feeder ever, has no output and no listeners. A listener
request for a path that has no output, and whose parameters name a `fallback`
that has something to send, creates the mount's output, fed by that fallback,
and the listener is admitted to it ([listeners.md §1](listeners.md#1-the-admission-pipeline)).
Icecast serves the fallback at the mount's URL in the same way. No source-connect
event fires: there is no feeder. A source client MAY take the mount later
([§3](#3-the-format-match-rule)). Every other request for a mount without
output answers `404`.

## 2. What feeds a mount

A mount has at most one feeder at a time.

- **A source client** takes a free mount through the admission of
  [sources.md §3](sources.md#3-admission).
- **A Liquidsoap source** is attached by the script, through the server's
  `attach_source` method with the mount path and the Liquidsoap source, and
  detached with `detach_source` ([parameters.md](parameters.md)). Its content
  MUST be encoded, in a container the server can deliver; the mount's format
  is that of the encoded content ([§3](#3-the-format-match-rule)). The script
  MAY pass the stream identity ([sources.md §5](sources.md#5-stream-identity-and-content))
  with it.

Rules for an attached Liquidsoap source:

- Attaching to a mount that has a feeder, or to a reserved path
  ([sources.md §2](sources.md#2-reserved-paths)), raises a named error kind
  ([parameters.md](parameters.md)). A source client that finds the mount taken
  by a Liquidsoap source is refused as for a source client ([sources.md
  §4](sources.md#4-replies)).
- It is not authenticated and not counted by `max_sources`: it is the
  script's own.
- Attaching and detaching raise the connect and disconnect callbacks, events
  and notifications of a source client ([sources.md §8](sources.md#8-lifecycle)).
  The source-connect event carries the Liquidsoap source of the feed.
- It is a feeder for as long as it is attached. While it has no data to give,
  the mount follows its `fallback` when one has something to send; with none,
  the mount output stays, its listeners stay connected and receive nothing, and
  the stream resumes when the source has data again (the same harbor
  capability as [§1.1](#11-linger)). Nothing is shut down and no
  new output is created. Rationale: the attachment, not the momentary flow,
  says whether the script still feeds the mount; a Liquidsoap source may pause
  or fail and recover, where a source client's loss is a disconnection.

A Liquidsoap source cannot be a preroll: a preroll is bytes
([prerolls.md](prerolls.md)), and a Liquidsoap source would need encoding.

## 3. The format match rule

The server never decodes or re-encodes ([sources.md
§5](sources.md#5-stream-identity-and-content)), so the streams a mount
switches between must be packet-compatible. A mount has a **format**: the
container and, for each stream, its codec. Bit rate, metadata and Ogg stream
serials are not part of it. Sample rate and channel count are part of it only
for a container that fixes them in one header written once (Matroska and WebM,
native FLAC): a listener who joined cannot be told of a change there. MP3 and
ADTS frames, and Ogg links, carry their parameters in the stream and players
follow a change.

A candidate (a source client, an attached Liquidsoap source, or a fallback)
**matches** when it has the same format.

- A fallback that does not match is skipped, as if it had nothing to send, and
  the skip is logged. A chain continues past it ([§6](#6-fallback)).
- An attached Liquidsoap source that does not match the format of the mount's
  existing output is refused with a named error kind
  ([parameters.md](parameters.md)): the script can handle it. A mount with no
  output takes any format.
- A source client is never refused for its format. When the mount has an
  output of another format, the output is replaced: it ends, its listeners are
  disconnected (they reconnect), and a new output is created with the new
  format.
- **Improvement.** Where the container lets a new stream start inside an
  existing connection, the output SHOULD continue with the new stream and keep
  its listeners instead of replacing the output. The server asks the copy
  encoder whether the new stream can continue the current one: the encoder
  knows the rules of each container (chained Ogg links, MP3 or ADTS frames with
  new parameters), and the server states none. Icecast, which relays bytes,
  ends the listeners' stream when its source goes away. A test asserts that a
  listener survives such a change. This is not subject to `compatibility`.

Rationale for the strict rule on fallbacks and attached sources: a listener
who joined a Matroska stream cannot follow another codec or layout, and a
fallback is switched in without its consent. Departure from Icecast, which
compares only the content type.

## 4. The mount output

Each mount has one output, created with its first feeder or by a listener
request that finds the fallback live ([§1](#1-lifecycle)), that delivers the
stream to its listeners. Delivery (sockets, bursts, ICY framing, time limits)
is the [harbor output's](../../../../core/outputs/spec/harbor/icecast-server.md).

Fixed when the output is created: the container and its MIME type, and
`dedicated_encoder` ([§5](#5-dedicated_encoder)). The stream identity headers
are not fixed: they follow the mount's current feeder and are built for each
reply ([sources.md §5](sources.md#5-stream-identity-and-content)). A source
client's identity is its own; an attached Liquidsoap source's is the `headers`
given to `attach_source`, none when there are none; while the mount follows a
fallback mount, it is that mount's feeder's, and none for a fallback that is a
Liquidsoap source; while a `linger` runs, it is the departed feeder's. The
parameters it reads when it is created, and those read at use, are
[parameters.md](parameters.md)'s ([§2.2](parameters.md#22-read-points),
[§4.1](parameters.md#41-fields)). The header list is
[parameters.md §4.2](parameters.md#42-headers), followed by the identity headers;
the rule that uses per-reply headers is
[listeners.md §3.2](listeners.md#32-per-reply-headers).

## 5. `dedicated_encoder`

`dedicated_encoder` gives each listener its own encoder, so each stream starts
with its own container header and a listener can join any streamable container
at any time. It costs one encoder per listener. It is a property of the mount
output, fixed at creation; its delivery is the harbor output's
([dedicated-encoder.md](../../../../core/outputs/spec/harbor/dedicated-encoder.md)).

## 6. Fallback

A mount's fallback is the `fallback` parameter: a mount (`mount`) or a
Liquidsoap source (`source`) ([parameters.md](parameters.md)).

- The mount follows its own feeder when it has one, and the fallback's
  otherwise. The switch happens at once, in both directions, at the next
  packet, not at a track end. Rationale: both are live encoded streams; there
  is no track to wait for and nothing to fade.
- **Improvement.** Listeners never leave their mount, and they always return
  to the mount's own feeder when it comes back. Their connection, the mount's
  headers, metadata and limits stay their own, and the switch is invisible to
  the player. Icecast moves listeners to the fallback mount and, with
  `fallback-override` off, leaves them there when the source returns; with the
  server reading the stream, nothing needs to move. This is not subject to
  `compatibility`, and `fallback-override` is ignored
  ([icecast-config.md](icecast-config.md#5-mount)).
- A fallback mount that gets a feeder later is picked up by every mount
  falling back on it; one that loses its feeder leaves them with none.
- A fallback must match the mount's format ([§3](#3-the-format-match-rule)).
- A reload that changes a mount's fallback applies to live mounts at once. A
  `fallback.source` MUST be created once, outside the function that builds the
  record ([parameters.md §9](parameters.md#9-overrides)): a source rebuilt at
  each reload has its own clock state, and a mount follows its new fallback by
  comparing sources by identity, so a rebuilt one is a different fallback.

### 6.1 Chains and loops

A fallback mount without a feeder of its own passes on to its own fallback,
and so on: the mount follows the first element of the chain that has
something to send and matches. Every `compatibility` value follows chains
(Icecast, Icecast-KH and RSAS all do).

- A chain MUST stop at a mount it already went through: a loop leaves the
  mount with nothing to send from that point.
- A chain MUST NOT be followed past 10 mounts. Rationale: Icecast's own bound.
  Under `rsas`, a listener request that follows a chain past the bound is
  refused `503` (RSAS reports "Maximum fallback recursion limit reached");
  under the other values the chain ends at the bound.
- A mount in the chain that has a feeder is used as is: its own fallback is
  not consulted.

### 6.2 `fallback_when_full`

[listeners.md §4](listeners.md#4-listener-limits) owns it: a listener refused
for `max_listeners` is redirected to the first non-full mount of this chain.

### 6.3 `fallbacks` admin request

`GET /admin/fallbacks?mount=<mount>&fallback=<mount>` (Icecast 2.4 and 2.5)
and `GET /admin/fallback?mount=<mount>&fallback=<mount>` (Icecast-KH) set the
mount's fallback (`mount`) at run time; both names are served under every
value, `rsas` included. RSAS has no such endpoint, so under `rsas` the request
is this server's rule, that of the mainline row below. The setting lives in the run-time overlay of
[parameters.md §8.4](parameters.md#84-run-time-overlay). Authentication and
errors are those of [sources.md §6](sources.md#6-admin-requests); a missing
`fallback` is `400`; an empty one clears the fallback.

| Value                                | The new fallback applies                         | Mount required to be live |
| ------------------------------------ | ------------------------------------------------ | ------------------------- |
| `icecast-2.5`, `icecast-2.4`, `rsas` | until the mount output ends or a reload          | yes                       |
| `kh`                                 | until a reload, whether the mount is live or not | no                        |

It takes effect at once, as a reload of the fallback does ([§6](#6-fallback)).
The reply is `200` with Icecast's `<iceresponse>` body.

## 7. Hidden mounts

A mount with `hidden` set is served and fed like any other but MUST NOT be
listed on the status pages or their JSON ([observability.md](observability.md)).
Listeners that know the path still reach it. `hidden` comes from the mount's
parameters ([parameters.md](parameters.md#41-fields)).

## 8. Metadata

- Metadata a feeder sends, in stream or through the admin endpoint
  ([sources.md §6.1](sources.md#61-metadata)), becomes the mount's current
  metadata, delivered to the mount's listeners in order with the audio.
- Listeners that asked for ICY metadata get the title as an ICY `StreamTitle`
  from `artist` and `title` ([harbor ICY](../../../../core/outputs/spec/harbor/icy.md));
  which mounts offer ICY titles is the harbor output's.
- Each update raises a metadata event with the mount, the metadata and the
  listener count, and a playlist log line when it has a title
  ([observability.md](observability.md)).
- While the mount follows a fallback, the metadata is that fallback's. After a
  switch, the current metadata is that of the feeder now followed, not a
  stale one.
- A mount's current metadata is exposed in its statistics.

## 9. Dump file

A mount with `dump_file` records its feeder's stream. Icecast's `<dump-file>`,
`<dump-file-size-limit>` and `<dump-file-time-limit>` are honoured under every
`compatibility` value.

- Recording starts when a feeder connects and ends when it leaves. The file
  holds the stream's encoded bytes in the container the feeder used. A
  fallback's stream is not recorded.
- `dump_file` is expanded with the calendar-time conversion of C's
  `strftime`, in local time, each time a recording starts. Rationale: Icecast
  names its dump files this way, so every feeder connection gets its own file.
- `dump_file_size_limit` (bytes) and `dump_file_time_limit` (seconds) stop the
  recording after the write that exceeds them. `0` or unset means no limit.
  Stopping the recording does not touch the stream.
- A file that cannot be opened or written is logged and the recording stops;
  the mount is unaffected.

## 10. Compatibility

| Rule                                                      | Values                                                                           |
| --------------------------------------------------------- | -------------------------------------------------------------------------------- |
| fallback chains ([§6.1](#61-chains-and-loops))            | all follow them; past the bound a listener request is refused `503` under `rsas` |
| `fallback_when_full` ([§6.2](#62-fallback_when_full))     | [listeners.md §4](listeners.md#4-listener-limits)                                |
| `fallbacks` request ([§6.3](#63-fallbacks-admin-request)) | `kh` applies it to a mount that is not live, until a reload                      |
| unknown mount on admin requests                           | [sources.md §6](sources.md#6-admin-requests)                                     |

Every other rule here holds under every value.

## 11. Conformance

**Binding**

- A mount appears with its first feeder and answers `404` before, except a
  configured mount whose fallback has something to send, which a listener
  request creates, fed by the fallback; a mount with nothing to send under any
  feeder is shut down and its listeners disconnected, whether or not a fallback
  is configured.
- A source client or Liquidsoap source can feed a mount; a second feeder is
  refused; attach and detach behave as [§2](#2-what-feeds-a-mount)
  says; a Liquidsoap source with no data lets the mount follow its fallback and
  takes it back, and with no fallback leaves the output and its listeners in
  place until data returns.
- Format match: a fallback of another format is skipped and an attached
  Liquidsoap source refused; a source client is accepted whatever its format
  and replaces the output ([§3](#3-the-format-match-rule)). **Improvement**:
  a source client that continues an Ogg stream or changes MP3 or ADTS
  parameters keeps the listeners.
- A fallback switch keeps the listener connection and headers, happens at once
  both ways, and applies to every mount sharing the fallback; the listeners
  always return to the mount's own feeder (**Improvement**, whatever
  `compatibility`); a reload or a `fallbacks` request changes it for live
  mounts.
- Chains are followed up to 10 mounts and stop at a loop; under `rsas` a
  listener request past the bound is refused `503`.
- A mount output keeps its listeners while its feeder is unavailable (linger,
  an idle attached Liquidsoap source) and is served again when the source is
  available; the server shuts it down itself.
- With `linger` set, a mount whose feeder leaves keeps its output and listeners
  for that long; a feeder arriving in the window continues the stream with the
  listeners attached; none arriving shuts the mount down at the end of the
  window. Without `linger` the mount shuts down at once.
- Parameters fixed at creation do not change under a live output, those read
  when used do.
- A hidden mount is served but unlisted; a metadata update reaches the ICY
  title, the event and the playlist log in order.
- Dump file: one file per feeder connection, name expansion, both limits stop
  the recording and not the stream.

**Incidental**: how soon the switch is seen (within a frame of the output),
ordering between mounts, the form of statistics, the file's byte layout beyond
being the feeder's stream.

**Doubles**: scripted source clients, a Liquidsoap source that can pause, a
second mount as fallback, a clock for dump limits.

Establish that tests ran: count the mounts created, feeders switched and
listeners kept, and fail when zero.
