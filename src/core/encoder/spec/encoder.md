# Encoders (Part A)

What every encoder offers, whatever its format. An encoder's own tree states
its format, options and bytes, and answers each face it implements.

## 1. Registry

- An encoder registers a plug-in that, given a format, answers a factory or
  nothing. The first factory answered for a format is used. A format no
  plug-in answers is a configuration error of the output that asked, at
  evaluation ([faults.md §2](../../../spec/faults.md#2-classes)).
- A factory takes the output's name and, for the stream face, the initial
  metadata, and creates an **instance**.

## 2. Instances

- An output never reuses an instance across runs. When it creates them
  within a run (at its start, per peer, per variant) is its own document's.
- Every instance serialises its own calls: two calls to one instance never
  run at the same time, whoever makes them.
- An instance is stopped exactly once
  ([concurrency.md §4](../../../spec/concurrency.md#4-resources)). Its stop
  drains it ([draining.md](../../source/spec/draining.md)) and returns its
  final bytes.
- Callers hand an instance zero-length pieces as well as full ones, and the
  metadata of a piece before the piece.

## 3. The stream face

What outputs that write a byte stream drive (`output.file`,
`output.icecast`, `output.harbor`, `output.srt`, ...):

- `encode(piece) → bytes`;
- optionally `encode_into(piece, consumer)`: encodes as `encode` does and
  hands the bytes to `consumer`, which copies them out during the call;
- `encode_metadata(metadata)`: in-stream metadata, effective in later
  output;
- `header() → bytes`: what a client joining now, at the start of the next
  output, needs first to decode; after a new logical stream began (an Ogg
  link after a metadata change), the new stream's header. It MUST be
  complete for every format the encoder produces, the container's
  identification headers included;
- `restart_offset() → offset?`: in the bytes the latest `encode` or `stop`
  returned, where a logical stream with its own header starts. It MAY be
  reported for the first output of an instance;
- `bitrate() → int?`;
- `stop() → bytes`: final bytes.

A stream joined mid-way carries nothing a joiner cannot decode: no ID3v2
tag or Xing frame in the middle of a live stream.

## 4. The segmenting face

What the HLS output drives: an encoder that also says where its bytes may be
cut. It is the
[HLS encoder seam](../../outputs/hls/spec/encoder-contract.md), with one file
per implementation.
