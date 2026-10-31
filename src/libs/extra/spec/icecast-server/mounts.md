# Mounts: lifecycle, output and fallback

A mount is a path that listeners read. It exists while a source feeds it or
while it serves as another mount's fallback. Sources are in
[sources.md](sources.md); listener admission in [listeners.md](listeners.md).
The `compatibility` setting (`"icecast-2.5"` default, `"icecast-2.4"`,
`"rsas"`, `"kh"`) chooses the implementation to mimic where they differ
([settings.md](settings.md)).

## 1. Lifecycle

- A mount is created when its first source connects.
- While it exists its source may change: the source leaves and returns, or
  the mount switches to its fallback ([§4](#4-fallback)). Listeners stay
  connected throughout.
- A mount that has neither a source nor a fallback source has nothing left to
  send and MUST be shut down: its listeners are disconnected and its path
  answers `404`. A source that returns later creates a new mount.
- A mount is also shut down when a reload removes its fallback and its source
  is gone.
- On server shutdown with sources connected, mounts end without hanging.

A mount listed only as a fallback, with no source ever connected, has no
output and no listeners; requests for it answer `404`.

## 2. The mount output

Each mount has one output, created with its first source, that delivers the
stream to its listeners. Delivery (sockets, bursts, ICY framing, time limits)
is the [harbor output's](../../../../core/outputs/spec/harbor/icecast-server.md).

Fixed when the output is created; a reload applies them to the mount's next
output ([settings.md](settings.md)):

- the stream identity headers of the source ([sources.md §4](sources.md#4-stream-identity));
- `http_headers`: the global ones, then the mount's, then the identity;
- `burst_size` and `burst_on_connect`, and `client_timeout`;
- the container, its `muxer_options` and the MIME type;
- `dedicated_encoder`.

Read later, at each listener admission or event, so a reload applies at once:
`max_listeners`, `max_clients`, listener decisions, `intro`, `preroll`,
`max_listener_duration`, `fallback_mount`, `on_connect_command` and
`on_disconnect_command`.

## 3. `dedicated_encoder`

`dedicated_encoder` gives each listener its own encoder, so each stream starts
with its own container header and a listener can join any streamable container
at any time. It costs one encoder per listener. It is a property of the mount
output, fixed at creation; its delivery is the harbor output's
([dedicated-encoder.md](../../../../core/outputs/spec/harbor/dedicated-encoder.md)).

## 4. Fallback

A mount's `fallback_mount` names a mount whose source takes over.

- The mount follows its own source when it has one, and the fallback mount's
  source otherwise. The switch happens at once, in both directions, at the next
  packet, not at a track end. Rationale: both are live encoded streams; there
  is no track to wait for and nothing to fade.
- Listeners never leave their mount. The mount's headers, metadata and
  limits stay its own.
- When the source returns, the mount returns to it. This is Icecast's
  `fallback-override` behaviour, always on: listeners are not moved between
  mounts, so there is nothing to move back.
- A fallback mount that gets a source later is picked up by every mount
  falling back on it; one that loses its source leaves them with none.
- A reload that changes `fallback_mount` applies to live mounts at once.
- Fallback chains: a fallback's own fallback is followed.

**Differences from Icecast.**

- `fallback-override`: Icecast moves the listeners of the fallback mount back
  to the original mount when its source returns, and only when the option is
  on. Here the listeners never moved, so the option has no meaning. Reason:
  the stream stays continuous on one connection.
- `fallback-when-full`: Icecast redirects a listener refused for
  `max_listeners` to the fallback mount. Here such a listener is refused as
  [listeners.md](listeners.md) states. Reason: a redirect would move the
  listener to another mount; `max_listeners` is not a fallback condition.
  Under `compatibility = "kh"` the redirect SHOULD be honoured (KH's
  `fallback-when-full`), by answering a redirect to the fallback mount.
- Icecast's `fallback-mount` on a fallback mount that itself is full is not
  followed.

## 5. Hidden mounts

A mount with `hidden` set is served and fed like any other but MUST NOT be
listed on the status pages or their JSON ([observability.md](observability.md)).
Listeners that know the path still reach it. Mainline, RSAS and KH all provide
`hidden`.

## 6. Metadata

- Metadata a source sends, in stream or through the admin endpoint
  ([sources.md §6](sources.md#6-metadata)), becomes the mount's current
  metadata, delivered to the mount's listeners in order with the audio.
- Listeners that asked for ICY metadata get the title as an ICY `StreamTitle`
  from `artist` and `title` ([harbor ICY](../../../../core/outputs/spec/harbor/icy.md)).
  Ogg mounts offer no ICY titles, as in Icecast.
- Each update raises a metadata event with the mount, the metadata and the
  listener count, and a playlist log line when it has a title
  ([observability.md](observability.md)).
- While the mount follows its fallback, the metadata is the fallback
  source's. After a switch, the current metadata is that of the source now
  followed, not a stale one.
- A mount's current metadata is exposed in its statistics.

## 7. Conformance

A test suite MUST establish:

- **Binding.** A mount appears with its first source and answers `404`
  before; a mount with no source and no fallback source is shut down and its
  listeners disconnected; a fallback switch keeps the listener connection and
  headers and happens at once, both ways; two mounts sharing a fallback both
  follow it; a reload of `fallback_mount` applies to live mounts; settings
  fixed at creation do not change under a live output, those read at
  admission do; a hidden mount is served but unlisted; a metadata update
  reaches the ICY title, the event and the playlist log in order; the
  `dedicated_encoder` property applies to the mount.
- **Incidental.** How soon the switch is seen (within a frame of the output),
  ordering between mounts, the form of statistics.
- Establish that tests ran: assert the number of mounts created, fallbacks
  switched and listeners kept, and fail when zero.
