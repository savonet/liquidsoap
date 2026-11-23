# The HLS output at the encoder seam

Part A. Seam view: how `output.file.hls` and its wrappers answer the caller
numbering of [encoder.md §11](encoder.md#11-caller-files). The operator's
rules are its tree's ([outputs/hls/spec/](../../outputs/hls/spec/README.md));
the segmenting face is
[encoder.md §4](encoder.md#4-the-segmenting-face) with its rules in
[encoder-contract.md](../../outputs/hls/spec/encoder-contract.md). This file
states only what binds the seam, and links the rest.

## 1. Formats and declarations

- Per variant, at construction, the factory is looked up; none is an
  invalid-value error naming the variant.
- **Declarations**: each variant's sink is `segmented(D)`, with `D` the
  segment duration; no reader starts mid-stream
  ([encoder.md §5.1](encoder.md#51-what-an-output-declares)).
- **Checks** ([encoder.md §5.3](encoder.md#53-the-check)): a configuration
  that is not segmentable is refused, as the HLS tree states
  ([hls variants.md §2](../../outputs/hls/spec/variants.md#2-building-a-variant)).
  The variant's ID3 mode, and the refusal of an `id3` field the
  configuration cannot honour, come from the configuration's declarations,
  read at evaluation without building a writer
  ([encoder.md §1](encoder.md#1-registry), item 4); no writer call
  initialises them. An explicit preamble
  (`%mp3(id3v2=…)`, an `%ffmpeg` `id3v2_version` above 0) is refused: the
  sink is segmented, and the ID3 the segments carry is the HLS contract's
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 5).

## 2. The sink

The variant's segmenter: it applies bytes and markers in order, cuts
segments at access points, and keeps each stream start's header as an init
segment ([hls segmenting.md](../../outputs/hls/spec/segmenting.md),
[files.md](../../outputs/hls/spec/files.md)). It never seeks.

## 3. Writer lifetime

One writer per variant per run, built at the run's start with its
`segmented(D)` capabilities, ended at the run's end
([hls operator.md §3](../../outputs/hls/spec/operator.md#3-lifecycle)).
Initial metadata: [output-encoded.md §3.2](output-encoded.md#32-initial-metadata);
the sink is segmented, so no writer writes it as tags
([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).

## 4. Pieces and metadata

Pieces are handed metadata first
([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)). No
writer is offered in-band metadata. Timed ID3 is an operation of the HLS
contract ([encoder-contract.md §7](../../outputs/hls/spec/encoder-contract.md#7-insert_id3),
[hls metadata.md §2](../../outputs/hls/spec/metadata.md#2-id3)), not the
in-band metadata of [encoder.md §9](encoder.md#9-in-band-metadata).

## 5. Markers and readers

Stream starts become init segments and restarts, access points become
split points ([encoder.md §4](encoder.md#4-the-segmenting-face)); the
segmenter places cuts among them
([hls segmenting.md §4](../../outputs/hls/spec/segmenting.md#4-the-cut-rule)).

## 6. End

A run that ends on a stop ends every writer `clean`: its final bytes and
access points are applied to the open segment before it closes
([encoder-contract.md §8](../../outputs/hls/spec/encoder-contract.md#8-stop)).
A run that ends on an error ends every writer `abandoned`, with its open
segment aborted ([hls operator.md §6](../../outputs/hls/spec/operator.md#6-errors-and-reopening)).
Every writer is ended exactly once.

## 7. Reopen, reconnect, append

Each run builds new writers, and every variant gets a discontinuity
([hls operator.md §6](../../outputs/hls/spec/operator.md#6-errors-and-reopening)).
Append: nothing.

## 8. Faults and restart policy

The writer reports typed faults ([encoder.md §8](encoder.md#8-faults)).
Item faults are dropped by the writer; an output fault of a writer ends the
run, and `reopen_on_error` is the one policy
([hls operator.md §6](../../outputs/hls/spec/operator.md#6-errors-and-reopening)).
A configuration error the writer reports at creation is a hard error.

## 9. Concurrency

Every writer call runs on the output's streaming work
([encoder.md §7](encoder.md#7-concurrency)); none runs at evaluation.

## 10. Tests

[hls conformance.md §4](../../outputs/hls/spec/conformance.md#4-encoder-seam).
Seam-binding:

- evaluating an HLS output builds no writer and starts no codec library;
- a variant whose configuration is not segmentable, or whose `id3` field
  it cannot honour, is refused at evaluation, naming the variant;
- after a streaming error, every writer of the aborted run was ended
  `abandoned` once, and the next run's segments decode on their own.
