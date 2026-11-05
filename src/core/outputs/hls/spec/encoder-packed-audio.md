# Packed-audio encoders with ID3 timestamps

Part A. `%mp3`, `%shine` and `%fdkaac` write raw MPEG audio or ADTS. They
use the adapter of [encoder-frame-units.md](encoder-frame-units.md) with
`init` and `insert_id3` replaced. Sections follow
[encoder-contract.md](encoder-contract.md).

## 1. Role

RFC 8216 §3.4 requires every packed-audio segment to start with an ID3 tag
holding its timestamp. These encoders build that tag.

## 2. Calls and order

As [encoder-frame-units.md §2](encoder-frame-units.md#2-calls-and-order).

## 3. `init`

ID3 mode prepended. `id3 = false` is rejected
([encoder-contract.md §3](encoder-contract.md#3-init)). `id3_version` is
recorded, default 3.

## 4. `init_encode`

As [encoder-frame-units.md §4](encoder-frame-units.md#4-init_encode).

## 5. `split_encode`

As [encoder-frame-units.md §5](encoder-frame-units.md#5-split_encode). MPEG
audio and ADTS frames decode on their own, bit reservoir aside.

## 6. Properties

As [encoder-frame-units.md §6](encoder-frame-units.md#6-properties):
`free_split` true. The formats give bit rate and codec string
([variants.md §3.1](variants.md#31-format-tables)), except `%mp3` VBR (no
bit rate).

## 7. `insert_id3`

Always returns a tag, also with empty metadata: ID3v2 of the recorded
version, with these frames:

1. One text frame per metadata entry whose key maps to an ID3 frame id
   (`title` → `TIT2`, …); others are dropped. Text is UTF-16 with BOM for
   version 3 and below, UTF-8 for version 4.
2. A `PRIV` frame: the owner `com.apple.streaming.transportStreamTimestamp`
   with its terminating zero byte, then the timestamp as 8 bytes
   big-endian, its upper 31 bits zero.

**Timestamp**: `round(time × 90000 / ticks per second) mod 2^33`, with
`time` the segment's start in ticks. Reason: the value is a 33-bit MPEG-2
timestamp; computing it from the tick count, not per frame, keeps it exact
when a frame is not a whole number of 90 kHz units. What is not promised:
the timestamp is the segment's start as the output counts it; an encoder's
own delay (priming, buffering) offsets the first sample from it by up to
that delay, which can differ from another variant's timestamps.

Example, the first bytes of a segment starting at 184 s, `%mp3`, metadata
`title = "T1"`, version 3:

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

The audio frames follow.

## 8. Header and stop

`header()` is empty. `stop()` is the encoder's own.

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
