# Known complexity

**What the history shows to be hard** in `input.srt`, `output.srt`, `srt.socket`
and the `settings.srt.*` settings, mined from commits, reverts, issues, pull
requests, regression tests and changelog entries. Each entry states a
difficulty without reference to the old code, so a rebuild can be checked
against it.

**Volume.** 198 distinct commits touched the operators, the script socket
value, the libsrt binding or the SRT tests (163 of them follow the operator
file through three moves since 2019). About 45 carried a behavioural fix or
a behaviour-revealing change; the rest were refactors, formatting, year bumps
and API adaptations, skipped unless they hid a libsrt change. Six were
reverts or same-week undo commits. 38 issues, pull requests and discussions
were read, including every issue the commits reference and the open and
closed issues that a search for "srt" returns. The binding's own changelog
(versions 0.1.0 to 0.3.4) was read for libsrt behaviour. Commit messages are
often terse ("Fix this.", "Better bis.", "Remove this."), so every fix was
read from its diff.

Entries are ordered by severity: hangs, deadlocks, crashes and data loss;
then stuck or unrecoverable states; then wrong behaviour; then leaks; then
smaller points.

---

## KC1. A connected peer that sends nothing hangs the reader

**Situation.** An input is connected (either role). The peer keeps the SRT
connection alive (keep-alive flow) but sends no payload, or sends it slower
than real time. The streaming thread reads from the socket to fill a frame.
The stop flag is consulted only between reads.

**What went wrong.** Before receive and send deadlines existed, the read
blocked forever: the clock stalled, stopping the operator had no effect, and
shutdown waited on it. The issue asked for two things: a deadline on every
socket operation, and readiness that reports "ready" only when there is data
to produce. Only the first was done. Readiness still means "connected", so a
silent peer now produces a read timeout per deadline, which ends the track,
tears the connection down and reconnects, in a loop.

**Why it is hard.** A live transport has three states the reader cannot tell
apart without a clock: data flowing, connected but idle, and gone. A blocking
read conflates them, and the flag that should end it is only seen when the
read returns. A deadline that is converted between units can silently become
"zero", which a library may read as "do not wait" or as "wait forever".

**How to tell.** Connect a peer that completes the handshake and then sends
nothing. Within the read deadline the input must stop claiming data, the
clock must keep ticking, and a stop or a process shutdown must complete
within one deadline. Run it with the deadline set to a value under one
millisecond and to `null`; both must behave as documented.

Sources: 4bcd5c70a, f605bb717, #2082, #2809.

**Spec:** Readiness means data available, the connection is kept ([input.md §3](input.md#3-readiness)); receives run off the streaming thread and are interrupted when the peer ends ([input.md §4](input.md#4-receiving-and-decoding)); durations round up, never to 0 ([common.md §1](common.md#1-parameters)).

---

## KC2. Blocking connection work on the thread or lock the stream needs

**Situation.** A caller attempts a connection (up to the connection
deadline per attempt, retried every polling delay) on the connect task.
Separately, the input builds its stream decoder on the first pull after a
connection, which may read the stream header from the socket.

**What went wrong.** The connection attempt held the operator lock for its
whole duration. The streaming thread asks the same lock whether the operator
is connected, so every attempt froze the clock for up to the connection
deadline, and with an unreachable host it froze it again every polling
delay. The fix moved the attempt out of the lock. Decoder creation was moved
the other way on purpose, from connect time to the first pull, so that
nothing reads the stream before the source is pulled; it now runs on the
streaming thread, where the header reads block the clock instead.

**Why it is hard.** "Is it connected?" is cheap and asked from the hot path;
"connect" is slow and asked from a background task. Guarding both with one
lock couples their latencies. Every place that may read from the network on
the streaming thread inherits the same coupling with its own deadline.

**How to tell.** Point a caller at an address that drops packets, with a
connection deadline of several seconds, inside a clock that also runs a
local source. The local source's output must not show gaps aligned with
connection attempts. Separately, connect a peer that sends its header
slowly: the clock's stall must be bounded by the read deadline and reported.

Sources: 7802d3e64, 8ece3ef9e.

**Spec:** The streaming thread never connects, sends or receives ([concurrency.md §1](concurrency.md#1-threads)); the decoder is created and fed by the reader ([input.md §4](input.md#4-receiving-and-decoding)); no user code under a lock ([concurrency.md §2](concurrency.md#2-callbacks-and-locks)).

---

## KC3. Script callbacks run while the operator lock is held

**Situation.** A listener accepts a client and fires `on_connect` (and on
disconnect, `on_disconnect` and the socket "close" event) while holding the
operator's lock. The lock is not reentrant. A synchronous script callback
calls back into the same operator, for example to list its sockets.

**What went wrong.** The callback waits for the lock its own thread holds:
the readiness loop stops, the streaming thread waits on the same lock, and
shutdown never completes. The caller role had the same shape and lost it
when the connection attempt left the lock; the listener kept it.

**Why it is hard.** The callback is the natural place to inspect the new
connection, and the lock is the natural guard for the client list the
callback wants to read. Callback code is the user's, so the operator cannot
know what it will touch.

**How to tell.** For every callback an operator fires, a synchronous
callback that calls every method of the same operator must return, in both
roles and both operator kinds. Then send the process its termination signal:
shutdown must finish.

Sources: 7802d3e64, 6d04b4ab5, 5845027c7.

**Spec:** No user code runs under an operator lock, and every method works from a callback ([concurrency.md §2](concurrency.md#2-callbacks-and-locks), [common.md §4](common.md#4-methods)).

---

## KC4. libsrt's process lifecycle: start late, start once, clean up last

**Situation.** libsrt has a process-wide start and cleanup, its own threads
(including one that delivers its log messages), and static state destroyed
at process exit. The program may evaluate scripts, fork to daemonize, run
`--check` or `--version`, and exit without ever building an SRT operator.

**What went wrong.** With libsrt started at program load, `liquidsoap
--version` and `--check` crashed at exit inside libsrt's static destructors,
and a daemonized process crashed right after the fork. Moving the start to
after daemonization fixed the fork; a crash on interrupt was still reported
afterwards. Later the start was made conditional on the first operator being
built, and then made safe against two threads building operators at once.
The input's buffer-overrun log was not connected to the operator's log for
months, and the binding's log handler had to be rebuilt so that
libsrt's threads never block on the host runtime.

**Why it is hard.** Library state outlives and precedes everything the
script controls. A fork duplicates libsrt's threads' state without its
threads. Exit ordering is chosen by the platform's loader, not by the
program. Lazy one-time initialization is easy to write and easy to race.

**How to tell.** With SRT built in: `--version`, `--check` on a script with
and without SRT operators, a daemonized run, an interrupt during streaming
and a clean stop must all exit with status 0 and no crash. Build SRT
operators from several threads at once: libsrt is started exactly once. With
the log route on, flood libsrt's log at debug verbosity during a send:
throughput must not drop.

(libsrt behaviour, from history) libsrt must not be started before a fork it
is expected to survive; its exit-time destructors crash when the start and
cleanup are unbalanced or happen in the wrong process; its log callback runs
on libsrt's own threads.

Sources: 3a6a98075, 1f26595f4, 383d1d8f9, 39ac9e13e, 08469c676, cf00f47ce,
b14d7fe43, fcecb8ea6, binding 0.2.2 and 0.3.1, #3450, #3473.

**Spec:** [library.md §1](library.md#1-initialisation): started once, after daemonising, only when used, balanced with cleanup, nothing called before the start; [§2](library.md#2-log-routing): the log handler never blocks libsrt's threads.

---

## KC5. State that belongs to the operator is reset with the connection

**Situation.** An input loses its connection and reconnects. Some state
lives exactly as long as one connection (socket, decoder, dump file); some
lives as long as the operator (the content buffer's layout, its size limit,
the decoder's working buffer within one connection). A disconnection can
happen while the buffered audio and video are of unequal length.

**What went wrong.** Resetting the buffer's content layout on every
reconnection crashed the input when audio and video were unequal at the
moment of disconnection; users saw an SRT input that never recovered inside
a fallback after its first reconnection. Setting the buffer's size limit
when the operator was built, before its content type was decided, crashed at
start-up (the same defect was found in a sibling input and kept as a
regression test). The decoder's working buffer was rebuilt on every frame
instead of once per connection.

**Why it is hard.** Reconnection is the moment when "reset everything"
looks safe, and the moment when partial data is most likely in flight. The
lifetime of each piece of state is a design decision that the code does not
state.

**How to tell.** Disconnect a video-and-audio input in the middle of a
frame, several times, inside a fallback: each reconnection must resume the
input and the fallback must select it again. Build an input whose content
type is decided late (a copy format): start-up must not fail. Each piece of
state must be documented with its lifetime (process, operator, connection,
frame) and a test must cover each reset point.

Sources: 83dda4672, fbff29210, c81ebf458, 5edc4d246, #1186, #1950.

**Spec:** State lifetimes are stated per piece ([input.md §2](input.md#2-lifecycle)); the buffer limit is set at wake-up; decoder, dump and socket live per peer ([input.md §6](input.md#6-reactions-to-peers)).

---

## KC6. Every connection is a new stream, and a shared encoder is not

**Situation.** An output encodes once and sends the bytes to one peer
(caller) or to several (listener). A peer disconnects and reconnects, or a
second listener client joins while the first is streaming. The input on the
other side builds one decoder per connection. Container formats such as Ogg
carry headers only at the start of a stream.

**What went wrong.** After a disconnection the output kept its encoder; the
new peer received the continuation of the old stream without headers, and an
Ogg/Opus receiver failed with "out of sync" on every reconnection, forever,
until the process was restarted. The fix restarts the encoder when a send
fails. It also stops the encoder for all listener clients when one fails, so
the others receive a new header in the middle of their stream; clients that
join a running listener get a stream from its middle. The final bytes that
stopping an encoder produces are queued and then discarded, so a clean stop
never sends the trailer. The input side ends the track when its decoder
fails and starts a new track with each new decoder.

**Why it is hard.** Transport (connections), encapsulation (headers,
trailers) and codec state are three layers with different lifetimes, and
one encoder feeding many connections forces them to share one. Some formats
tolerate joining mid-stream (MPEG-TS, MP3); others do not (Ogg), so a test
on the tolerant format passes.

**How to tell.** With an Ogg encoder: disconnect and reconnect the peer
twice; the receiver must decode both reconnections and start a new track at
each. With two listener clients: kill one; the other must not see a new
stream header. Stop the output cleanly; the peer must receive the encoder's
trailer. A third client joining mid-stream must either get the header or be
refused for formats that need it; which one is a spec decision.

Sources: 977e17429, 77cf76e88, 8ece3ef9e, cd78f705b, 682c85838, 801f29d54,
#4395, #4399.

**Spec:** Every peer's stream starts at the encoder's start or with its header ([output.md §4](output.md#4-encoder)); one peer's failure leaves the others' streams intact ([output.md §5](output.md#5-sending)); a clean end delivers the trailer ([output.md §6](output.md#6-stopping)). The late-joiner decision: header, then live.

---

## KC7. A connection can end between any two steps

**Situation.** The streaming thread decides the input is ready, then fetches
the socket, then reads. The peer closes in between, or the readiness loop
or a script call disconnects the operator concurrently. At the end of a
stream, the last receive returns the end-of-stream marker while bytes are
still buffered.

**What went wrong.** Fetching a socket that had just gone away raised
outside the error handling and crashed the frame. On end of stream, the
reader declared the end as soon as the receive reported it, losing the bytes
already buffered but not yet handed to the decoder.

**Why it is hard.** "Ready" is a snapshot. Any check-then-act sequence over
connection state is wrong without a lock or a re-check, and the error path is
exercised only when timing is unlucky.

**How to tell.** Under full CPU load, disconnect a streaming input from a
script thread in a loop: no crash, and every disconnection ends a track.
Send a known byte sequence followed by a close: the decoder receives every
byte, verified with the input's dump.

Sources: e04f05786, 64003622b.

**Spec:** Transitions re-check state atomically ([connection.md §2](connection.md#2-state)); readiness needs no socket lookup ([input.md §3](input.md#3-readiness)); a stale socket only yields an ignored failure ([concurrency.md §3](concurrency.md#3-shared-state)); end of stream is signalled after every received byte ([input.md §4](input.md#4-receiving-and-decoding)).

---

## KC8. Reset must be stop then start

**Situation.** The clock falls behind beyond its maximum latency and resets
every active output. An output's reset restarts its connection and encoder.

**What went wrong.** The output's reset was written as start then stop for
seven years, from the first version of the operator. After "Too much
latency! Resetting active sources...", the output reconnected and tore
itself down at once; the clock still believed it started, so nothing ever
restarted it and the stream stalled until the process was restarted. A
sibling output had copied the same inverted order.

**Why it is hard.** The path runs only under overload, which tests do not
create. The clock's view of the output (started) and the output's own state
(stopped) diverge silently, so no error is logged.

**How to tell.** Force a latency reset (block the clock for longer than the
maximum latency) on an output in each role. The peer must receive data again
within one reconnection delay, and the output's reported state must match
whether it sends.

Sources: 664d649f0 (origin), ce0f954ee, 1a539eb90, #5309.

**Spec:** Reset is stop then start, leaving the output started and connecting ([output.md §3](output.md#3-lifecycle)).

---

## KC9. The listening socket outlives its clients

**Situation.** A listener binds once and accepts clients. A client
disconnects, fails a send, or is closed by the script. The operator stops,
or the process shuts down. The readiness loop's registrations are one-shot:
a ready socket leaves the set and its handler must register it again.

**What went wrong.** The listening socket was torn down and rebuilt on every
client disconnection, as users reported, and the output listener admitted a
single client. Tying its lifetime to "the operator
stops" fixed both. An accept that fails before the listening socket is
registered again leaves the listener out of the readiness loop until the
next connect. A client accepted while the operator was stopping was left
open.

**Why it is hard.** The input and output share one listener role, but a
single-client input made "tear down on disconnect" look acceptable. One-shot
readiness means every path out of the accept handler, error paths included,
must re-arm.

**How to tell.** Connect, disconnect and reconnect a caller fifty times
against a listener: every attempt connects, and the listening socket's
identity (as reported to `on_socket`) never changes. Make one accept fail
(reject in the admission predicate, abort the handshake): the next caller
still connects. Accept during stop: no socket is left open.

Sources: c02ecbed2, cd78f705b, 38b029286, d98c597a9, 1f423a495, da559204f,
#2556.

**Spec:** The listening socket lives as long as the connection is wanted ([listener.md §2](listener.md#2-state)); registrations persist and a failed accept leaves the listener armed ([listener.md §4](listener.md#4-connect), [poll.md §2](poll.md#2-registering-and-removing)); an accept landing after release is closed ([connection.md §2](connection.md#2-state)).

---

## KC10. The readiness loop's view of its set drifts from libsrt's

**Situation.** One process-wide readiness loop waits on libsrt's readiness
set, with a table mapping each registered socket to its handler. Sockets are
added from script threads, the streaming thread and the loop's own handlers;
they are closed by any of them. Sockets in the set are non-blocking; out of
it they must be blocking.

**What went wrong.** A count of registered sockets kept beside libsrt's set
drifted: libsrt removes closed sockets silently, and the loop then got
"empty set" errors with a non-zero count. Waiting on an empty set is an
error, not a wait. Removing a socket that is not in the set is an error.
Re-arming added a new table entry each time instead of replacing it, which
grew without bound. The lock around the table was removed in a refactor and
never restored. An unexpected error, including a handler missing for a
socket that was just removed, puts the loop to sleep until the next
registration, freezing every listener in the process. Where the blocking mode is
switched back on moved between three places over three commits; the loop's
own description still claims a place that no longer does it.

**Why it is hard.** Two owners hold the same set: libsrt mutates it on its
own (closing removes), the program mutates it from several threads, and the
loop's error paths are only hit when they disagree. Any shadow state kept
next to libsrt's set is a second truth.

**How to tell.** Under load, open and close a hundred callers against two
listeners while a script stops and starts one of them: the second listener
must accept throughout, the loop must never sleep while its set is
non-empty, and the registration table's size must return to the number of
listening sockets. Every socket handed to the stream must be in blocking
mode.

(libsrt behaviour, from history) Closing a socket removes it from every
readiness set without notice; waiting on an empty set returns an error at
once; removing an absent socket is an error; a socket accepted from a
non-blocking listening socket inherits non-blocking mode.

Sources: 1f423a495, 9fc663746, c0c74f2f2, 192bbae15, 483706c95, d98c597a9,
6b7236def, ad62e02ad, fdb783b97, 7d120e137.

**Spec:** One owner for the set and table, persistent registrations, tolerant removal, a loop that never sleeps while its set holds a socket ([poll.md](poll.md)); the blocking mode is set by the socket's owner ([poll.md §2](poll.md#2-registering-and-removing)).

---

## KC11. Shutdown must reach every operator, and must not keep any alive

**Situation.** The process shuts down while callers retry connections, a
listener is armed and outputs are sending. Operators may be built
dynamically by scripts, many times over a long run.

**What went wrong.** Shutdown had to be tightened so that callers stop
retrying and every operator disconnects. Each operator then registered its own hook in
the process-wide lifecycle, which kept every dynamically built operator
alive forever (a memory leak listed in the changelog). The hooks became one
process-wide flag plus a registry that does not keep operators alive.

**Why it is hard.** Whatever is registered to be told about shutdown is
referenced by the registry, and a reference is a lifetime. The only time the
registry is read is at shutdown, so a leak shows only as slow growth.

**How to tell.** Build and drop a thousand SRT operators from a script: live
memory returns to its baseline after a collection. With callers pointed at a
dead host, shutdown completes within one connection deadline.

Sources: 1f26595f4, 67d0ecf95, 4d223afca, CHANGES.md 2.2.4 ("dynamically
created sources").

**Spec:** One shutdown step: the flag, then a bounded release of every operator, found through a registry that does not keep them alive ([library.md §4](library.md#4-shutdown), [connection.md §9](connection.md#9-stopping)).

---

## KC12. A connection attempt and a disconnection race

**Situation.** The caller's attempt runs on the connect task. Meanwhile the
script calls `disconnect()` or `connect()`, the output's send fails and
reconnects, or the input's decoder fails and reconnects, each from another
thread. The attempt fires `on_connect` and then publishes the socket.

**What went wrong.** The socket is published only after `on_connect` has
run, so the stream cannot use a connection before its callbacks have set up
(this ordering was fixed deliberately). The task handle and the attempt in
flight are not covered by the disconnection: an attempt that completes after
a disconnect stores a socket and fires `on_connect` after `on_disconnect`,
leaving a stopped operator connected. `connect()` on a connected caller
replaces the socket without `on_disconnect` and without restarting the
output's encoder.

**Why it is hard.** Connection state has at least four values (idle,
attempting, connected, closing) but is often stored as "socket or nothing"
plus a separate task handle. Every transition comes from a different thread.

**How to tell.** From a script thread, call `disconnect()` at random points
during attempts against a slow peer, a thousand times: after each, the
operator holds no socket and the last event fired is `on_disconnect`. Call
`connect()` twice in a row: exactly one connection and one task exist.

Sources: 7802d3e64, 7d120e137, 977e17429, a4f2f4e3f.

**Spec:** The caller's state is one value with four states, and attempts carry the identity of the transition that started them ([caller.md §2](caller.md#2-state)); `connect()` while active does nothing ([caller.md §4](caller.md#4-connect)).

---

## KC13. Who paces the clock, and when

**Situation.** A connected live-mode SRT input delivers data at the sender's
pace and can pace its clock; a file-mode sender pushes faster than real
time and must not. A disconnected input paces nothing. An output's sends do
not block on a timer, so it never paces. Several SRT inputs may share a
clock, inside a fallback that switches between them and local sources.

**What went wrong.** Whether the input paces its clock was changed at least
eight times: tied to the connection state, fixed to "yes", back to the
connection state in the same week, then made optional for file-mode streams, then reported as one process-wide sync
source shared by all SRT inputs. A dedicated clock shared by all SRT inputs
was added and removed two weeks later. The clock rebuild made the input
announce the change at connect and disconnect, and moves the clock to a
thread while SRT paces it, because the input then blocks on its socket.

**Why it is hard.** Pacing depends on the sender's mode, which the receiver
cannot observe, and on the connection state, which changes while the clock
runs. Two inputs blocking on two sockets are two pacers even when they
report the same identity.

**How to tell.** Connect, disconnect and reconnect a paced input inside a
fallback with a local source: the stream position stays continuous across
each change and the clock logs who paces it. Feed a file-mode stream with
pacing off: the clock does not overrun. Put two connected inputs in one
clock: the conflict is reported, not hidden.

Sources: ce0f954ee, 5c7819152, 5e8951ad7, 48cc9d7b2, fa52a0deb, 55fc9d6f2,
01d559687, 0cd96e4f9, 646a82198, c31ab058d, 21ebfe7c1, 923ae2ef7, 24084719f,
96e8df380, b8ee50ee7, a0c56dc00, 22bb40a97, #4364.

**Spec:** An input paces only while its connection is time-controlled, with the stream as the sync source's identity ([input.md §7](input.md#7-pacing)); the output never paces ([output.md §8](output.md#8-pacing)).

---

## KC14. Connection callbacks: pairing, order and thread

**Situation.** The operator reacts to connections itself (opening the dump,
declaring the sync source, closing the decoder, resetting the encoder) and
scripts register their own `on_connect`, `on_disconnect` and `on_socket`
callbacks. Callbacks may run synchronously or on another thread.

**What went wrong.** The caller fired `on_disconnect` on every failed
attempt, without a preceding `on_connect`, once per polling delay; then it
fired it only when a socket existed, while the listener still fires it on
every disconnect, even with no client, and never when it loses one client.
Callbacks were briefly run in reverse registration order, so script
callbacks ran before the operator's own reactions. Callbacks moved to
other threads by default, so a script callback could observe state the
operator had not finished changing; tests had to be switched to
synchronous ones and the choice was then made explicit. A script that registers callbacks from a function that
runs repeatedly piled them up until registration returned a release.

**Why it is hard.** The operator's internal reactions and the user's
callbacks share one mechanism, so their relative order and thread are
implicit. "Disconnected" means different things for one peer and for many.

**How to tell.** For each role and kind, record every event: they strictly
alternate `on_connect`/`on_disconnect` per peer, never start with
`on_disconnect`, and the operator's own reactions are visible to a
synchronous script callback (the dump file is open in `on_connect`). A
failed attempt fires nothing but a log line.

Sources: a4f2f4e3f, a26e0933b, 6d04b4ab5, ebf419934, 329387501, 82dcc264c,
aa8183484.

**Spec:** Events pair per peer ([connection.md §8](connection.md#8-disconnect)); the operator's own reaction precedes callbacks ([common.md §3](common.md#3-callbacks)); order per operator and what synchronous holds back ([concurrency.md §2](concurrency.md#2-callbacks-and-locks)).

---

## KC15. Where a socket option is set decides whether it works

**Situation.** Options can be set on the caller's socket before connecting,
on the listening socket before listening, or on each accepted socket after
accept. Scripts set their own through `on_socket`. Encryption needs both
ends to agree; passphrases have length limits.

**What went wrong.** Setting latency on an accepted socket instead of the
listening socket made the connection time out; the script hook had to expose
the listening socket for that. A passphrase shorter than 10 or longer than 79
characters was rejected by libsrt with "Operation not supported: Bad
parameters" while the clock started the listener, surfacing as "Source
failed while starting" with a stack trace. A connection deadline was set on
the listening socket, where it means nothing, and then removed. Accepted
sockets had to be put back in blocking mode because they inherit the
listening socket's mode.

**Why it is hard.** libsrt negotiates some options at handshake time, so an
option set after accept is too late, and an option set on the listening
socket is silently inherited. The library's errors do not name the option.

**How to tell.** For each documented option, a test sets it through the
operator parameter and through `on_socket` in each mode, connects, and reads
it back on both ends. A passphrase of 9 and 80 characters fails at script
evaluation with a message naming the parameter.

(libsrt behaviour, from history) Accepted sockets inherit the listening
socket's options, including blocking mode, encryption and latency; latency
must be set before the handshake; passphrases must be 10 to 79 bytes; the
connection deadline applies to callers only.

Sources: 3560f33d4, a8723b72d, d98c597a9, 4bcd5c70a, 11eef57ce, 192bbae15,
#3520, #4256, #4257, #4325.

**Spec:** The role's options are set before `on_socket`, which comes before connect or bind ([connection.md §3](connection.md#3-socket-setup)); accepted sockets inherit the listening socket's options ([listener.md §3](listener.md#3-socket-setup), [library.md §7](library.md#7-libsrt-behaviour-relied-upon)); passphrase and other values validated at evaluation ([common.md §1](common.md#1-parameters)).

---

## KC16. Address resolution and dual stack

**Situation.** The caller resolves a host name; the listener resolves its
bind address. Hosts may resolve to IPv4 and IPv6 addresses in either order.
Platforms differ (Windows).

**What went wrong.** Taking the first address of a host-only lookup ignored
IPv6. Resolving with system preferences made "localhost" resolve to IPv6
while the listener bound the IPv4 wildcard, so every SRT test had to pin
IPv4. Binding an IPv6 address accepted IPv4-mapped clients until the
listener set IPv6-only by default for IPv6 binds. A resolution path through
low-level address structures crashed on Windows and was replaced by the
platform's standard resolver.

**Why it is hard.** The default addresses of the two roles ("localhost" and
the IPv4 wildcard) are not of the same family on a modern host, and the
failure is a silent connection timeout.

**How to tell.** With default parameters on a dual-stack host, a caller
reaches a listener on the same host under each preference (system, IPv4,
IPv6). An IPv6 bind with IPv6-only unset rejects IPv4 clients. The tests run
without pinning the family.

Sources: 3472313eb, cfdc1c7b2, 8b3df606d, 255be7d06, 0021e959c, #4317,
#4538, #4542.

**Spec:** The caller tries every resolved address in order ([library.md §5](library.md#5-address-resolution), [caller.md §4](caller.md#4-connect)); IPv6-only default for IPv6 binds ([listener.md §3](listener.md#3-socket-setup)); tests run without pinning ([conformance.md §1](conformance.md#1-principles)).

---

## KC17. Long-blocking tasks in a shared worker pool

**Situation.** The readiness loop waits up to its poll timeout per
iteration; each caller's connect task blocks up to its connection deadline
per attempt. Both run on the scheduler's worker pool, shared with request
resolution, servers and clocks.

**What went wrong.** Their placement in the pool changed three times: moved
to the general queue, reverted after queue starvation deadlocks elsewhere in
the scheduler, then reclassified when the pool's classes were renamed.

**Why it is hard.** Ten callers pointed at dead hosts hold ten workers for
their whole deadline. A bounded pool turns that into starvation of unrelated
work, which shows far from SRT.

**How to tell.** With more failing callers than the pool has workers, a
harbor or telnet request is still answered within its usual time.

Sources: fcd8bf488, b18d14a52, c9ca3eb3a, 8e4e38fb2, #4151, #4219, #5405.

**Spec:** No blocking call holds a shared bounded worker ([concurrency.md §1](concurrency.md#1-threads)); the readiness loop has a thread of its own ([poll.md §1](poll.md#1-model)).

---

## KC18. Half-built sockets and per-connection resources leak

**Situation.** Building a connection takes several steps (create, set
options, call the script hook, bind or connect, listen or accept, register).
Any step can fail. A connection also owns a decoder and possibly a dump
file.

**What went wrong.** A failed caller attempt, a failed bind and a failure
after accept each left the new socket open; a client accepted while stopping
was not closed. The decoder was dropped without being closed on
disconnection. The readiness table grew by one entry per re-arm.

**Why it is hard.** Each step adds one more resource to release on every
later failure, and the failure paths run only against misbehaving peers or
bad parameters.

**How to tell.** For each step, inject a failure (bad address, port in use,
script hook that raises, handshake rejected, stop during accept) a thousand
times: the process's open socket and file descriptor counts return to their
baseline.

Sources: 7802d3e64, 7d120e137, 1f423a495, 05715ef44, fdb783b97,
CHANGES.md 2.2.5 ("listen socket polling callbacks").

**Spec:** Every socket closed exactly once on every path, with its close event when it had a creation event ([connection.md §3](connection.md#3-socket-setup)); every encoder stopped exactly once ([output.md §4](output.md#4-encoder)); decoder and dump closed with the peer ([input.md §6](input.md#6-reactions-to-peers)).

---

## KC19. Errors reach the script, against the right argument

**Situation.** Parameters are validated at construction, socket options are
read and written from scripts at run time, and libsrt raises its own errors.

**What went wrong.** A missing decoder was reported against an empty
argument name. An invalid mode said "Invalid mode!" without the valid
values. Reading an option on a closed socket raised an internal error until
library errors were converted to catchable script errors, a conversion
redone twice since. Timeouts were in milliseconds while every other
duration was in seconds.

**Why it is hard.** Errors cross three layers (libsrt, operator, script) and
each layer has its own error kind; a missed conversion shows only on the
path that fails.

**How to tell.** Every socket method called on a closed socket, and every
method of an unconnected operator, raises a catchable script error or
returns an empty value as documented. Every invalid parameter fails at
evaluation and names the parameter and its valid values.

Sources: 2dd7f12f8, c89f05345, 6e1afcd00, 45489de6c, 433ba80a2, f605bb717,
#2809.

**Spec:** Validation names the parameter and its valid values ([common.md §1](common.md#1-parameters)); `sockets()`, `connect()` and `disconnect()` never raise; socket methods raise kind `srt` ([failures.md §7](failures.md#7-script-calls)).

---

## KC20. Tests that pass without proving the behaviour

**Situation.** The SRT tests run both ends in one process on fixed local
ports, in parallel with other tests.

**What went wrong.** The reconnection regression test was hardened and then
skipped the same day, so the Ogg reconnection fix (KC6) is untested. The
copy-encoder start-up test passes as soon as the script starts. The raw PCM
test passes on the first track mark, which the input adds when it builds a
decoder, before any byte is decoded. The passphrase test has no negative
case, so it also passes if encryption is ignored on both ends. Ports were
randomised once to avoid clashes and later fixed again. All tests pin IPv4
(KC16). A user reported that the operators failed to connect to each other
while third-party SRT tools worked with either end.

**Why it is hard.** The cheapest pass signal (a callback fired) is emitted
before the behaviour it stands for. Testing an implementation only against
itself hides interoperability defects on both sides at once.

**How to tell.** Each test asserts on data that crossed the connection
(byte count or decoded content), has a negative twin that must fail
(mismatched passphrase, rejected stream id), and runs once against a
third-party SRT peer. A skipped test is reported as missing coverage.

Sources: 682c85838, 801f29d54, c81ebf458, 1e6abc293, e96cf1c74, 329387501,
3472313eb, #2661.

**Spec:** [conformance.md](conformance.md): data-crossing assertions, negative twins, a third-party peer, skipped checks reported.

---

## KC21. A low-bitrate stream is held back by a fixed payload size

**Situation.** An output sends its encoder's bytes in payloads of a fixed
size. The encoder's output rate drops far below its usual rate for a while:
a lossless codec compressing digital silence, typically at the end of a
track or between tracks. The receiver waits for data with a deadline.

**What went wrong.** FLAC compresses digital silence to a few hundred bytes
per second; a 1316-byte payload then took several seconds to fill. The
receiver got nothing for longer than its one-second read deadline, timed out
and reconnected, and then received the held bytes as a burst. Its buffer
emptied and refilled, and the stream played faster for a moment. A user
worked around it by adding inaudible noise to the signal, which keeps the
encoder's rate up but makes the source never silent, so a fallback can no
longer switch away from it.

**Why it is hard.** A fixed payload size is a sound default at normal
bitrates and an unbounded delay at low ones; the bitrate is the encoder's
and the content's, not the transport's. Tests use constant tones or
high-bitrate codecs, which fill a payload in a few milliseconds.

**How to tell.** Send a FLAC encoding of silence for several seconds. Every
payload must arrive within the delay bound of the bytes it carries, plus a
scheduling margin; the receiver must not time out.

Sources: field report from a production deployment, 2026-11 (FLAC over SRT,
with and without Ogg).

**Spec:** Covered: the delay bound ([output.md §5](output.md#5-sending)),
checked by [conformance.md](conformance.md).

---

## Part B notes

- The binding must copy a send buffer before releasing the runtime lock; the
  collector may move it while libsrt reads it (binding 0.3.4).
- libsrt's log callback must not enter the runtime; the binding queues
  messages outside the managed heap (binding 0.2.2).
- Forcing a lazy value from several threads is unspecified in OCaml; the
  one-time start of libsrt needed a mutexed suspension (39ac9e13e).
- An inherited default method (`self_sync` on the output class) hid the
  need to override it in every output (96e8df380).
- Replacing a mutex by an atomic counter dropped the protection of the
  handler table, which the atomic did not cover (9fc663746).
- `Hashtbl.add` on re-registration keeps the old binding; `replace` is
  needed (fdb783b97).
- Catching `Not_found` where the getter raises `Not_connected` makes
  `sockets()` raise on an unconnected input caller (cd78f705b to now).
- The posix-socket resolution path passed an `NI_` constant as `ai_flags`
  and crashed on Windows; the stdlib `Unix.getaddrinfo` replaced it
  (3472313eb to 0021e959c).
- A process-wide shutdown registry must hold operators weakly (67d0ecf95,
  4d223afca).
