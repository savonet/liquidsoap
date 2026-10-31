# Icecast server: future work

Features that servers in this family have and that the specification leaves
out, and enhancements Liquidsoap can make on its own. Each entry says what the
feature is, which server has it, which Liquidsoap pieces would build it, and
what stops it today. None of them is a requirement; the other documents link
here instead of describing them.

## Medium

**`killclient`.** Disconnect one listener by its identifier, from the admin
interface (`/admin/killclient?mount=&id=`). Icecast and Icecast-KH have it.
Building blocks: the listener record's `id` and the admin handlers
([observability.md](observability.md)). Blocker: the harbor output has no
operation to disconnect a single listener.

**CORS preflight.** Icecast 2.5 answers an `OPTIONS` request on a mount with
`204` and the CORS headers a `GET` would get, so browsers can check a
cross-origin player before connecting
([listeners.md](listeners.md)). Building blocks: the per-reply headers the
server already computes. Blocker: the harbor output serves `GET` on a mount
only.

**Postroll at the time limit.** RSAS plays an audio file to a listener whose
time limit ends, before closing the connection. It is a static `<postroll>`
with `<max-listener-duration>`, or a webhook `postroll` header sent after
`icecast-auth-timelimit`. Building blocks: the preroll
mechanism in reverse ([prerolls.md](prerolls.md)), the listener time limit.
Blocker: the harbor output closes at the limit; it has no way to send bytes
first.

**Relays.** A mount fed from another server's stream, either always connected
(persistent) or only while a listener is present (on demand). Icecast, KH and
RSAS have them. Building blocks: `input.http` in copy mode feeding a mount
([sources.md](sources.md), [mounts.md](mounts.md)), `source.dynamic` for on
demand. Blocker: the on-demand start needs a "first listener arrived" hook from
the mount output, and the reconnect policy is not specified.

**Looped-file fallback.** A fallback that is a file played again and again, as
Icecast's `fallback-mount` pointing at a file does. Building blocks: a
Liquidsoap source as a fallback ([mounts.md](mounts.md)), `single` or
`playlist` in copy mode. Blocker: the format-match rule: the file must already
be in the mount's container and codec.

**Midrolls.** RSAS inserts an advertisement into a running stream. Triggers are
`RSAD#####` metadata (milliseconds, in artist or title) and its Manage API
(`POST /<mount>/manage` with `action=midroll`); after a trigger RSAS POSTs to
the `<midroll-webhook>` URL (`action=midroll_req`) to fetch the JSON ad list. Building
blocks: `switch` or `fallback` between the mount's source and an ad source
decoded and encoded to the mount's format, the metadata event. Blocker: copy
mode cannot cut a stream at an arbitrary point; the ad needs a decode and
re-encode path, which the mount output does not offer.

**HLS from the same mounts.** RSAS serves an HLS version of a mount (per-mount
`<hls>`, packed audio in AAC ADTS or MP3, no Ogg). Building
blocks: `output.harbor.hls` fed by the mount's source. Blocker: a mount carries
encoded packets, HLS segments need them cut at keyframes and packaged; the
mapping from a mount to an HLS output is not defined.

**YP directory listing.** Icecast and KH announce public mounts to directory
servers (`yp-url`) and refresh the entry. Building blocks: the `public` and
stream identity statistics, `http.post`. Blocker: the YP protocol is not specified here and no
Liquidsoap client for it exists.

**Same-port TLS detection.** Serve TLS and plain HTTP on one port by peeking
at the first byte (`0x16` is a TLS handshake). Icecast 2.5 does it with `auto`,
its default (`auto_no_plain` answers plain requests with `426`), and
Icecast-KH whenever a certificate is configured. Building blocks: the
transport value. Blocker: the harbor transport reads a connection as one
protocol only; until then the configuration translation rejects such files.

**Listener latency choice (RSAS).** RSAS takes `?latency=high|low` on a listener
request in place of a burst setting. Building blocks: the request's query
string in the listener decision, per-mount `burst_size`. Blocker: the burst is
fixed when the mount output is created, not per listener.

**`drop_existing_listener` (KH).** With the per-user limit, drop the older
connection of the user instead of refusing the new one. Building blocks: the
per-user decision ([listeners.md](listeners.md)). Blocker: no operation to
disconnect one listener (see `killclient`).

**`max-bandwidth` (KH).** Refuse or limit listeners when the outgoing rate
passes a ceiling. Building blocks: the `bytes_sent` of the listener record, a
listener decision. Blocker: the server has no rate measurement over a window,
and the harbor output has no rate limiter.

## Large

**Move a listener into another mount's output.** One harbor operation that
moves an admitted listener, or all those of a mount, into another mount's
output without disconnecting them. It serves two features: `moveclients` and
`movelisteners` (Icecast, KH and RSAS have them), and `fallback_when_full`
served in place with a single authentication, where the server now redirects
([listeners.md §4](listeners.md#4-listener-limits)). Building blocks: the
listener record's `id`, the admin handlers. Blocker: listeners belong to one
harbor output; they cannot move to another.

**Master and slave (KH).** A master redirects listeners to slaves to offload
streaming, and slaves relay the master's mounts. Building blocks: relays, the
redirect outcome of the listener decision ([listeners.md](listeners.md)).
Blocker: the relay protocol between master and slave (a stream list, a
credential exchange) and a load measure per slave.

## Liquidsoap-only enhancements

Things the server could offer that no other server in this family does, because
mounts are Liquidsoap sources.

**Transcoded sibling mounts.** A mount `/live.mp3` and the same stream
re-encoded at `/live.opus` or at a lower bitrate. Building blocks: the mount's
source decoded and fed to another mount. Blocker: the decode path is outside
the copy-only mount output.

**Spliced midrolls and prerolls in any format.** An advertisement or jingle
decoded, crossfaded with the stream and encoded in the mount's format.
Building blocks: `fallback` with transitions, the encoder of the mount.
Blocker: as for midrolls; prerolls also need decoded audio spliced into a
per-listener encoder. `dedicated_encoder` gives each listener an encoder, but
the preroll path sends bytes and has no way to feed it ([prerolls.md](prerolls.md)).

**Crossfaded live takeover.** When a source client connects, fade from the
fallback into it instead of cutting. Building blocks: `fallback` transitions.
Blocker: needs decoded audio on both sides.

**Metadata rewriting hook.** A callback that edits the metadata of a mount
before listeners and logs see it (clean titles, add the station name).
Building blocks: `metadata.map` on the mount's source, `on_metadata`.
Blocker: where in the mount output the hook runs, so that the ICY title and the
log agree.

**Metadata updates on Ogg and WebM mounts.** The admin metadata update is
refused for containers that carry their metadata in the stream
([sources.md §6.1](sources.md#61-metadata)). The server reads the stream, so it
could write the update into it (a new Ogg link, a new tag). Blocker: the
output cannot yet start a new link on request, and what a player does at
that point is untested.

**Recording and time shift.** Keep the last hours of a mount and serve them
from an offset. Building blocks: `dump-file`, `output.file`, `input.http`
or `playlist` on the recording. Blocker: serving a file as a live stream with a
start position is not an Icecast notion; the interface is to design.

**Generated prerolls.** A preroll made per listener (text to speech, a jingle
chosen by the listener's country). Building blocks: the listener decision
returning preroll bytes ([listeners.md](listeners.md)), an external generator
with the cache helper ([prerolls.md](prerolls.md)). Blocker: generation latency
inside the admission path; needs a deadline and a fallback to no preroll.
