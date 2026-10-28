# ICY metadata

In-stream metadata for Shoutcast-style clients: after every `metaint` bytes of
stream data, the server inserts one metadata block.

## 1. Negotiation

A listener gets ICY metadata exactly when its request has a header named
`icy-metadata` (any case) whose value, with surrounding whitespace removed,
is a non-zero decimal integer, and the output's MIME type is not an Ogg one.
Then the response head carries `icy-metaint: <metaint>`
([connection.md §5](connection.md#5-the-response-head)) and blocks are
inserted for this listener. Otherwise the listener gets the plain stream.
Negotiation is per listener and fixed for the connection.

**Ogg.** The MIME type is taken up to its first `;`, trimmed and lowercased.
When it is `application/ogg`, `audio/ogg` or `video/ogg`, the request header
MUST be ignored: no `icy-metaint`, no blocks. Ogg carries metadata in band,
and clients that always request ICY (Icecast relays, GStreamer's Ogg demuxer)
do not strip it; Icecast never sends ICY on Ogg either. The decision is made
once per output, from the resolved MIME type.

No other format is excluded. `settings.harbor.icy_formats` concerns sources
sending metadata to the server and does not apply here.

## 2. Block format

A block is one length byte `n` followed by `16 × n` bytes. The empty block is
the single byte `0x00`.

A block carries the text:

```
StreamTitle='<title>';StreamUrl='<url>';
```

Each part is present only when its value exists: no title gives no
`StreamTitle` part, a null `url` gives no `StreamUrl` part. When neither
exists the text is empty and the block is the empty block. Values are
written as they are: Shoutcast-style clients read a value up to the closing
`';`, and no escaping convention is shared between them.

For a text of `len` bytes, `n = ceil(len / 16)`, and the block is `n`, the
text, then zero bytes up to `16 × n`. A text MUST NOT exceed
`MAX_TEXT = 255 × 16 = 4080` bytes, the most one length byte can describe.

**Truncation**, before encoding:

1. A url longer than `MAX_URL` bytes (recommended 200) is cut to fit
   `MAX_URL` with `...` appended.
2. The title is then cut, with `...` appended, so that the whole text fits
   `MAX_TEXT`.

A cut that would split a UTF-8 sequence of a valid UTF-8 value MUST move back
to the start of that sequence, so the value stays valid UTF-8. A value that is
not valid UTF-8 is cut on bytes.

**Worked examples.**

- Title `icy test`, no url. Text `StreamTitle='icy test';` is 23 bytes;
  `n = 2`; the block is `0x02`, the 23 bytes, 9 zeros: 33 bytes.
- Title `abcdefghijklmnopq` (17 bytes), no url. Text is 32 bytes; `n = 2`;
  the block is `0x02` and the 32 text bytes: 33 bytes.
- Artist `A`, title `T`, url `http://x`. Text
  `StreamTitle='A - T';StreamUrl='http://x';` is 41 bytes; `n = 3`; the block
  is `0x03`, the text, 7 zeros: 49 bytes.
- A 4000-byte title and a 300-byte url. The url becomes 197 bytes plus `...`;
  the text without the title is `15 + 13 + 200 = 228` bytes, so the title is
  cut to 3849 bytes plus `...`; text 4080 bytes, `n = 255`.

## 3. Title composition

From the metadata, keys `artist` and `title`:

| artist | title | title text       |
| ------ | ----- | ---------------- |
| set    | set   | `artist - title` |
| set    | —     | `artist`         |
| —      | set   | `title`          |
| —      | —     | none             |

No other key is consulted. The url always comes from the `url` parameter.

## 4. Cadence and change detection

**Position.** Per listener: the number of stream bytes sent since the last
block. A block immediately precedes the stream byte that follows every
`metaint`-th stream byte: when the position has reached `metaint`, the next
stream byte is preceded by a block and the position restarts at 0. A block
is sent together with, or before, the stream byte it precedes, never on its
own at the end of the available data; the byte stream is the same either
way once more data arrives. The response head is not counted; the mode's
initial bytes (codec header, burst) are counted like any stream data.

**Metadata events.** Every metadata the source produces is one event, with a
generation number, increasing per output, and its converted values
([§5](#5-charset)). Two events with the same contents are still two events:
a title produced twice is announced twice. The output's **last metadata** is
the latest event; it starts unset and survives a restart.

**Stamping.** Each stream byte has the metadata **in effect** for it: the
latest event produced before that byte was encoded. A block carries the
metadata in effect for the stream byte it precedes:

- a non-empty block built from it, when it is set and its generation differs
  from the last one sent to this listener;
- the empty block otherwise.

The first block a listener receives carries the metadata in effect at that
point, if any. The title therefore changes with the audio, whether the
listener is live, lagging by up to `buffer`, or reading a burst. How each
mode knows the metadata in effect for a byte is its own
([shared-encoder.md §4](shared-encoder.md#4-flush),
[dedicated-encoder.md §5](dedicated-encoder.md#5-encode-and-send)).

**Framing is never broken.** The ICY framing counts bytes from the start of
the body: a stream byte dropped or a block misplaced corrupts every later
boundary for the rest of the connection. A listener MUST receive every
stream byte from its join point on, in order, with a block exactly where the
position says, or be disconnected ([delivery.md §6](delivery.md#6-disconnection)).

## 5. Charset

Every metadata event is converted, value by value, from an auto-detected
charset to the charset named by `encoding` (UTF-8 when empty). Values
detected as binary are left unchanged; a value whose conversion fails is kept
as it was, with a log line. Keys are not converted.

ICY blocks are built from the converted values, and the encoder's in-stream
metadata is the same converted values with cover art removed
([modes.md §6](modes.md#6-metadata)).
