# ICY metadata

In-stream metadata for Shoutcast-style clients: every `metaint` bytes of stream
data, the server inserts one metadata block.

## 1. Negotiation

A listener gets ICY metadata exactly when its request has a header named
`icy-metadata` (any case) whose value is exactly the string `1`. Then the
response head carries `icy-metaint: <metaint>`
([connection.md §4](connection.md#4-the-response-head)) and blocks are
inserted for this listener. Otherwise the listener gets the plain stream.
Negotiation is per listener and fixed for the connection.

ICY is offered whatever the stream's format; the setting
`settings.harbor.icy_formats` concerns sources sending metadata to the server
and does not apply here.

## 2. Block format

A block is one length byte `n` followed by `16 × n` bytes. An empty block is
the single byte `0x00`.

A non-empty block carries the text:

```
StreamTitle='<title>';StreamUrl='<url>';
```

Each part is present only when its value exists: no title gives no
`StreamTitle` part, a null `url` gives no `StreamUrl` part. Values are written
as they are, with no escaping of `'` or `;`.

Encoding, for a text of `len` bytes:

- `n = floor(len / 16) + 1`;
- the block is `n`, then the text, then zero bytes up to `16 × n`.

A text whose length is a multiple of 16 therefore gets 16 trailing zeros, and
an empty text (metadata present, but neither a title nor a url) gives
`n = 1` and 16 zeros.

**Truncation.** Before encoding, a title longer than 3852 bytes is cut to its
first 3849 bytes followed by `...`; a url longer than 200 bytes is cut to its
first 197 followed by `...`. Cuts are on bytes, not characters. The intent
recorded in the code is to keep the whole text within the format's limit of
`255 × 16 = 4080` bytes.

**Worked examples.**

- Title `icy test`, no url. Text `StreamTitle='icy test';` is 23 bytes;
  `n = 2`; the block is `0x02`, the 23 bytes, 9 zeros: 33 bytes.
- Title `abcdefghijklmnopq` (17 bytes), no url. Text is 32 bytes; `n = 3`;
  the block is `0x03`, 32 text bytes, 16 zeros: 49 bytes.
- Artist `A`, title `T`, url `http://x`. Text
  `StreamTitle='A - T';StreamUrl='http://x';` is 41 bytes; `n = 3`; block of
  49 bytes.

## 3. Title composition

From the metadata, keys `artist` and `title`:

| artist | title | title text       |
| ------ | ----- | ---------------- |
| set    | set   | `artist - title` |
| set    | —     | `artist`         |
| —      | set   | `title`          |
| —      | —     | none             |

No other key is consulted. The url always comes from the `url` parameter, never
from metadata.

## 4. Cadence and change detection

Per listener, the output keeps a position: the number of stream bytes sent
since the last block. When the position reaches `metaint`, the next byte sent
is a block, and the position resets to 0. The response head is not counted;
the mode's initial bytes (codec header, burst) are counted like any stream
data.

Per output, the output keeps the **last metadata**: the latest metadata the
source produced, after charset conversion ([§5](#5-charset)). It starts
unset and survives a restart.

At each boundary, the block sent is:

- a non-empty block built from the last metadata, when the last metadata is
  set and is not the same value as the last one sent to this listener;
- an empty block otherwise.

"The same value" is identity of the stored metadata, not equality of
contents: the source producing the same title twice yields two distinct
values, so the block is sent again. The first boundary of every listener
carries the last metadata, if any.

Which last metadata is read at a boundary depends on the mode: the value
current when the data is flushed to the socket in the shared mode
([shared-encoder.md §4](shared-encoder.md#4-flush)), the value current when
the data is encoded in the dedicated mode
([dedicated-encoder.md §5](dedicated-encoder.md#5-encode-and-send)).

## 5. Charset

Every metadata the source produces is converted, value by value, from an
auto-detected charset to the charset named by `encoding` (UTF-8 when empty).
Values detected as binary are left unchanged; a value whose conversion fails
is kept as it was, with a log line. Keys are not converted.

The converted metadata is what ICY blocks are built from. In the shared mode
it is also what the encoder receives as in-stream metadata, with cover art
removed ([shared-encoder.md §6](shared-encoder.md#6-metadata)).
