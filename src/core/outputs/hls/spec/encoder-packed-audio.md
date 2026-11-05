# Packed-audio encoders with ID3 timestamps

Part A. As built. `%mp3`, `%shine` and `%fdkaac` write raw MPEG audio or
ADTS. They use the adapter of
[encoder-frame-units.md](encoder-frame-units.md) with two calls replaced:
`init` and `insert_id3`. Sections follow
[encoder-contract.md](encoder-contract.md).

## 1. Role

RFC 8216 §3.4 requires every packed-audio segment to start with an ID3 tag
holding its timestamp. These encoders build that tag.

## 2. Calls and order

As [encoder-frame-units.md §2](encoder-frame-units.md#2-calls-and-order).

## 3. `init`

Records `id3_version`. If `id3_enabled` is `false`: error `Format requires
ID3 metadata!` at the format's position, which stops the script. Returns
`true`.

## 4. `init_encode`

As [encoder-frame-units.md §4](encoder-frame-units.md#4-init_encode).

## 5. `split_encode`

As [encoder-frame-units.md §5](encoder-frame-units.md#5-split_encode). MPEG
audio and ADTS frames decode on their own, so every chunk start is a valid
split point in practice, bit reservoir aside.

## 6. Properties

As [encoder-frame-units.md §6](encoder-frame-units.md#6-properties). The
formats themselves give bit rate and codec string
([variants.md §3.1](variants.md#31-format-tables)), except `%mp3` VBR (no
bit rate).

## 7. `insert_id3`

Always returns a tag, also with empty metadata. The tag is ID3v2 of the
recorded version (default 3) with these frames:

1. One text frame per metadata entry whose key maps to an ID3 frame id
   (`title` → `TIT2`, …); others are dropped. Text is UTF-16 with BOM for
   version 3 and below, UTF-8 for version 4.
2. A `PRIV` frame, owner `com.apple.streaming.transportStreamTimestamp`,
   then a zero byte, then the timestamp as 8 bytes big-endian.

Frames carry their id's default flags. **Timestamp**, in 90 kHz units:
`frame_position × trunc(frame duration × 90000) + trunc(sample_position in
seconds × 90000)`, reduced modulo `0x1FFFFFFFF` (finding F-ID3-1).

Example, the first bytes of a segment starting at 184 s, `%mp3`, metadata
`title = "T1"`, version 3 (checked):

```
49 44 33 03 00 00 00 00 00 50                    "ID3" v3.0, flags 0, size 80 (syncsafe)
54 49 54 32 00 00 00 07 c0 00                    "TIT2", size 7, flags c0 00
01 ff fe 54 00 31 00                             UTF-16, BOM, "T1"
50 52 49 56 00 00 00 35 c0 00                    "PRIV", size 53, flags c0 00
63 6f 6d 2e 61 70 70 6c 65 2e 73 74 72 65 61 6d  "com.apple.stream
69 6e 67 2e 74 72 61 6e 73 70 6f 72 74 53 74 72  ing.transportStr
65 61 6d 54 69 6d 65 73 74 61 6d 70 00           eamTimestamp" 0
00 00 00 00 00 fc af 80                          16560000 = 184 s × 90000
```

The audio frames follow at once (here a LAME header frame).

## 8. Header and stop

`header()` is empty for all three. `stop()` is the encoder's own.

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
