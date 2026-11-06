# Tests

The suite runs both ends in one process: an `input.srt` and an `output.srt`
over the loopback interface. Each test is a script that passes by calling the
harness's pass function and fails on timeout or on an uncaught error. Test
names below are a trace back to the source, not content.

## 1. The checks

| Invariant                                                                    | Binding                                                                                                                                                                                           | Incidental                                                                                         | Trace                                                                                                   |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| A listener's admission predicate receives the stream id the caller set.      | The predicate is called during the handshake with `streamid` equal to the caller's string.                                                                                                        | The predicate rejecting afterwards; the WAV format; the one-second delay before the caller starts. | `srt_listen_callback`                                                                                   |
| An output listener accepts several callers at once and reports each.         | `on_connect` fires once per accepted caller: two callers, two events.                                                                                                                             | Format, port.                                                                                      | `srt_multiple_outputs`                                                                                  |
| Encrypted connections succeed when both ends set the same passphrase.        | `on_connect` fires on the listener. No mismatched case: the test also passes if neither end encrypts.                                                                                             | The passphrase's value beyond its length.                                                          | `srt_passphrase`                                                                                        |
| A stream sent by the output reaches the input's decoder.                     | A track starts on the input. The track mark is added when the decoder is created, before any byte is decoded: this proves a connection and a decoder found for MIME parameters, not decoded data. | The codec, sample rate, layout.                                                                    | `srt_raw_pcm`                                                                                           |
| Disconnecting and reconnecting an input caller starts a new track each time. | More than one track seen across two cycles.                                                                                                                                                       | The delays.                                                                                        | `GH4395` — **skipped**: the script calls the skip function before doing anything, so it checks nothing. |
| An output with a copy encoder on a fallible source is accepted.              | Construction does not fail.                                                                                                                                                                       | The unreachable HTTP source.                                                                       | `ffmpeg-copy-input-http`                                                                                |

The documentation examples `srt-sender.liq` and `srt-receiver.liq` are
typechecked only.

## 2. Harness

- Each test binds a fixed port: 8001–8004, 9502, 10001.
- Every streaming test sets `settings.srt.prefer_address` to `"ipv4"`, so
  `localhost` resolves to the address the listener bound, `0.0.0.0`.
- No failure is simulated: no dropped peer, no slow peer, no timeout, no bad
  passphrase.
- Doubles a new harness would need: an SRT peer that can be told to stop
  reading, to vanish without closing, to send a stream id, to use a wrong
  passphrase; a way to observe the exact bytes of each payload.

## 3. Untested

Nothing checks: the payload cut ([output.md §10](output.md#10-wire-bytes)),
the encoder trailer on stop, a late listener client, peer failure for either
role, caller reconnection after a lost peer, the read timeout ending a track,
`sockets()`, `connect()`/`disconnect()` on the output, the socket value's
methods, the settings, IPv6, `max_clients`, a passphrase mismatch, the
callback lock, or shutdown with connections open.
