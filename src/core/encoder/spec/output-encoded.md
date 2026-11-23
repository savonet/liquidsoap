# The encoded-output base

Part A. What every output that drives a container writer shares, whatever
its sink: how a gathered frame becomes pieces and metadata, which metadata
an output holds as current, the exported form a writer is given, and the one
order in which in-band metadata, out-of-band metadata and pieces leave the
output. It answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files) for what holds at this level;
each caller file answers the same numbers for its own output and links here.

It relies on: the output's states, its streaming cycle and its reset
([output.md](../../outputs/spec/output.md)); metadata and track marks in a
frame ([source-contract.md §3–§4](../../source/spec/source-contract.md#4-metadata));
the writer, its creation and its in-band metadata
([encoder.md](encoder.md)); out-of-band placement
([sink.md §5](sink.md#5-out-of-band-metadata)); faults
([faults.md](../../../spec/faults.md)).

Outputs that use this base: `output.file` ([output-file.md](output-file.md)),
`output.external` ([output-pipe.md](output-pipe.md)), `output.icecast`,
`output.harbor`, `output.srt`, `output.url`, the HLS output.

## 1. Formats and declarations

Nothing at this level: the lookup, the declarations and the check of
[encoder.md §5](encoder.md#5-capabilities-and-the-checks-at-evaluation) are
each output's §1.

## 2. The sink

Nothing at this level.

## 3. Writer lifetime

### 3.1 Current metadata

1. An output holds a **current metadata**: the metadata in effect at the
   position the output has reached in its stream. It is the output's view of
   its source's last metadata
   ([source-contract.md §4](../../source/spec/source-contract.md#4-metadata)):
   - a metadata at position `p` of a frame becomes current from `p`; several
     metadata at one position are merged in frame order, a later one winning
     key by key;
   - a track mark at position `p` with no metadata at `p` empties it from
     `p`. Reason: a new track that carries no metadata is not the previous
     track, and a file or a link labelled with the previous one misleads.
2. The current metadata is kept in full. Its exported form (§4.2) is what
   writers get; the full form is what the output's own uses read (a file
   name, a script callback), unless a caller file says otherwise.
3. It survives a stop, a reopen and a retry: a destination opened later
   starts with the metadata in effect at its first piece.

### 3.2 Initial metadata

A writer's initial metadata ([encoder.md §2.2](encoder.md#22-creation)) is
the exported form of the current metadata at the position of the writer's
first piece, the metadata at that position, if any, applied first. That
metadata is not also offered in band (§4.3). Since it is then the current
metadata, an offer of it is an unchanged offer under
[encoder.md §9](encoder.md#9-in-band-metadata) item 3: an implementation
MAY get this rule from that item alone, without tracking whether a writer
has had a piece.

## 4. Pieces and metadata

### 4.1 Cutting a frame into pieces

On each streaming cycle where the output hands a gathered frame on
([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)):

1. The frame's filled length `n` is cut at every distinct metadata position
   `p` with `0 < p < n`. Each interval `[a, b)` with `a < b` is a **piece**.
   No piece is empty. Positions are in main ticks; the writer counts samples
   and images ([encoder.md §3.3](encoder.md#33-frames-in)).
2. The metadata at a piece's start, merged as in §3.1, is that piece's
   metadata. Every metadata of the frame belongs to exactly one piece, except
   one at position `n`.
3. A metadata at position `n` labels the first sample of the next frame: the
   output keeps it, and hands it with the first piece of the next frame it
   hands on, merged under that frame's own metadata at 0. It becomes current
   only then.
4. Track marks do not cut pieces, and a writer never sees them
   ([encoder.md §3.3](encoder.md#33-frames-in)). They change only the
   current metadata (§3.1).
5. A frame left short by a fallible source is cut over its filled length. A
   frame left short by an infallible source is the backstop of
   [source-contract.md §11](../../source/spec/source-contract.md#11-concurrency),
   not a case of this base.

Worked example: a frame of 1764 ticks with `{title=A, foo=x}` at 0,
`{on_air=…}` at 900 and `{title=B}` at 1764:

| Piece                    | Metadata of the piece                                |
| ------------------------ | ---------------------------------------------------- |
| `[0, 900)`               | `{title=A, foo=x}`, exported `{title=A}`             |
| `[900, 1764)`            | `{on_air=…}`, exported empty                         |
| next frame's first piece | `{title=B}`, merged under that frame's metadata at 0 |

### 4.2 The exported form

What a writer is given is the **exported form** of a metadata:

1. A key is kept when its lower-cased form is in
   `settings.encoder.metadata.export`. A kept key keeps its case.

   | Setting                            | Default                                                                                                                                                                   |
   | ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
   | `settings.encoder.metadata.export` | `artist`, `title`, `album`, `genre`, `date`, `tracknumber`, `comment`, `track`, `year`, `dj`, `next`, `apic`, `pic`, `metadata_url`, `metadata_block_picture`, `coverart` |
   | `settings.encoder.metadata.cover`  | `pic`, `apic`, `metadata_block_picture`, `cover`, `coverart`                                                                                                              |

2. Every exported key that carries a picture or another binary payload is in
   the cover list. Keys of the cover list are removed, unless the output's
   `export_cover_metadata` is true. Reason: a picture per track is large,
   and an output that does not embed it pays for it at every metadata
   ([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)).

   | Output                          | `export_cover_metadata`           |
   | ------------------------------- | --------------------------------- |
   | `output.file`                   | script parameter, default `true`  |
   | `output.external`, `output.url` | script parameter, default `false` |
   | every other output              | `false`, not a parameter          |

   Whether a writer writes the metadata it is given as tags, cover art
   included, is the tag rule of
   [encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7:
   by default a file gets its tags and its cover, a stream and a continued
   file get neither. Reason for `output.file`'s default: a file is kept,
   and its cover is part of what a library shows.

3. The exported form can be empty. An empty exported form is never offered to
   a writer (§4.3); the output's own uses of the full metadata still see the
   metadata.

### 4.3 Order

For each piece, in frame order, the output's streaming work:

1. if the piece has a metadata, makes it current (§3.1); runs the caller's
   own reaction to it (a reopen, a callback), as its caller file states;
   then
   - offers the exported form to the writer in band, when the writer offers
     in-band metadata, the output chose in band
     ([encoder.md §9](encoder.md#9-in-band-metadata)), the exported form is
     not empty, and the piece is not the writer's first (§3.2; an offer of
     the first piece's metadata is unchanged under
     [encoder.md §9](encoder.md#9-in-band-metadata) item 3, so that rule
     alone MAY meet this condition);
   - hands the metadata to the sink out of band, when the output carries
     metadata out of band ([sink.md §5](sink.md#5-out-of-band-metadata));
2. calls `write(piece)`.

This one order serves both channels: an in-band offer precedes the piece it
labels, and out-of-band metadata handed after the previous `write` returned
is placed after every byte written so far
([encoder.md §3.2](encoder.md#32-order-of-a-calls-output)). No output needs
another order.

## 5. Markers and readers

Nothing at this level.

## 6. End

Nothing at this level. A metadata kept for the next frame (§4.1 step 3) when
the output stops becomes current, so that the next destination starts with
it.

## 7. Reopen, reconnect, append

Nothing at this level beyond §3: a new writer's initial metadata is the
current metadata at its first piece, whatever caused the new destination.

## 8. Faults and restart policy

- A writer call or a sink that reports an output fault ends the frame for
  that destination: the pieces of the frame after the fault reach no writer
  of the failed destination. The output's restart policy decides what
  follows ([encoder.md §8](encoder.md#8-faults)).
- A script function a caller runs at step 1 of §4.3 that raises is logged at
  `severe` with its backtrace, and counts as its caller file states
  ([faults.md §0](../../../spec/faults.md#0-the-rule)).

## 9. Concurrency

Everything here runs on the output's streaming work, inside its cycle
([encoder.md §7](encoder.md#7-concurrency)). A request from another thread
(a stop, a reopen) is carried out between two pieces, never inside one.

## 10. Tests

- **Pieces are cut at metadata and nowhere else.** For frames with metadata
  at 0, mid-frame, twice at one position, and at the frame's end: the number
  of `write` calls equals the number of distinct metadata positions strictly
  inside `(0, n)` plus one; no piece is empty. Needs a source emitting
  metadata at chosen offsets and a recording writer.
- **A metadata at the frame's end is not lost**: it is handed with the first
  piece of the next frame.
- **Two metadata at one position** reach the writer as one, merged.
- **Order.** The recording log shows each in-band offer and each out-of-band
  hand-off before the `write` of the piece it labels, and after the previous
  `write`.
- **Exported keys.** For each output and both values of
  `export_cover_metadata`: the keys a writer receives are exactly the
  exported set; no key of the cover list reaches a writer when the option is
  false; `on_air` never does with the default settings.
- **Initial metadata.** After `{title=A}`, a destination opened by any cause
  starts a writer whose initial metadata carries `title=A`; after a track
  mark without metadata, it is empty.
- **No first-piece offer.** A writer whose first piece carries a metadata
  receives it as initial metadata and no `metadata` call for it.
