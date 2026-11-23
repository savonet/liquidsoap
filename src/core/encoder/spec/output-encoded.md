# The encoded-output base (as built)

What every output that drives a stream-face encoder inherits: how a gathered
frame is cut into pieces, in which order metadata and pieces reach the
output's own `encode_metadata`, `encode` and `send`, and which metadata keys
survive the way. Inheritors: `output.file`, `output.external`
([output-file.md](output-file.md), [output-pipe.md](output-pipe.md)),
`output.icecast`, `output.harbor`, `output.srt`, `output.url`, the HLS output.

It relies on: the generic output (states, the streaming cycle that gathers a
frame, reset, failure), owned by
[output.md](../../outputs/spec/output.md) and not restated here; the
instance record, owned by the seam [encoder.md](encoder.md).

The base does not hold an encoder. It declares three operations the inheritor
supplies, and calls them:

| Operation            | Given                          | Returns                                                                                                                            |
| -------------------- | ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------- |
| `encode_metadata(m)` | exported metadata (§3.2)       | nothing                                                                                                                            |
| `encode(piece)`      | a frame from 0 to its position | a value of the inheritor's choosing: bytes (file, external, icecast), nothing (SRT, HLS, `output.url`), a deferred action (harbor) |
| `send(v)`            | what `encode` returned         | nothing                                                                                                                            |

Whether and when an encoder instance exists, and what is done with the bytes,
is each inheritor's.

## 1. Formats accepted and how the factory is obtained

Nothing at this level: the base never sees a format or a factory.

## 2. Instance lifecycle

Nothing at this level. The base adds one parameter to every inheritor:
`export_cover_metadata` (§3.2), fixed by the inheritor (see the table in §3.2)
or exposed to the script.

## 3. Call sequence

### 3.1 Cutting a frame into pieces

On each streaming cycle where the generic output hands a gathered frame on
(`send_frame`, [output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)),
and only when the frame's position is greater than 0:

1. Build the list `0 :: sort(positions of every metadata in the frame ++
[position of the frame])`. Positions are in main ticks. Duplicates are
   kept.
2. Walk consecutive pairs `(start, stop)` of that list:
   - `start = stop`: nothing (a metadata at the frame's position, or two
     metadata at one position, or a metadata at 0);
   - `start < stop`: one **piece** `[start, stop)`, processed as in §3.3.
3. When one element is left, it is the frame's position. For an infallible
   output the code asserts it equals the full frame size; a partial frame
   from an infallible source fails this assertion before the generic
   output's own "infallible source produced a partial frame" check runs.

Consequences:

- Pieces are never empty: `encode` is never called with a zero-length piece
  by this base.
- Track marks do not cut pieces. A piece may contain a track mark; the
  inheritor and the encoder see it only inside the piece.
- A metadata at the frame's position (its end) belongs to no piece and is
  never handed on.
- Each piece starts either at 0 or at a metadata position, so each piece
  carries at most one "own" metadata, at its offset 0.

### 3.2 Which metadata is handed on, and in what form

For the piece `[start, stop)`, the base looks for the **first** metadata, in
the frame's own list order, whose position `p` satisfies
`start ≤ p < stop`. Only one is handed on per piece; a second metadata at the
same position is dropped.

That metadata is first stripped to the **exported** keys:

- the export list is `settings.encoder.metadata.export` (default: `artist`,
  `title`, `album`, `genre`, `date`, `tracknumber`, `comment`, `track`,
  `year`, `dj`, `next`, `apic`, `pic`, `metadata_url`,
  `metadata_block_picture`, `coverart`); a key is kept when its lower-cased
  form is in the list. The kept key keeps its original case;
- when the inheritor's `export_cover_metadata` is false, the keys listed in
  `settings.encoder.metadata.cover` (default `pic`, `apic`,
  `metadata_block_picture`, `cover`) are removed from the export list first.
  `coverart` is exported but not in the cover list, so it survives either
  way.

| Inheritor                                            | `export_cover_metadata`            |
| ---------------------------------------------------- | ---------------------------------- |
| `output.file`, `output.external`                     | script parameter, default **true** |
| `output.url`                                         | script parameter                   |
| `output.icecast`, `output.harbor`, `output.srt`, HLS | false                              |

The keys the generic output adds to every metadata (`on_air`,
`on_air_timestamp`) are not in the default export list, so they are never
handed on unless a script adds them.

The result may be empty (a metadata carrying only non-exported keys). The base
still calls `encode_metadata` with the empty set; what happens next is the
inheritor's (pipe outputs call their `reopen_on_metadata` with it, then hand
nothing to the encoder, [output-file.md §5](output-file.md#5-reopen-reconnect-append-file-naming)).

### 3.3 Order within a piece

The base has an **encoding order**, `metadata_first` by default; an inheritor
may switch it once at construction. Only `output.icecast` does, to
`data_first`, when it sends ICY metadata.

| Order            | For each piece                                                               |
| ---------------- | ---------------------------------------------------------------------------- |
| `metadata_first` | `encode_metadata(m)` if a metadata was found; `v = encode(piece)`; `send(v)` |
| `data_first`     | `v = encode(piece)`; `send(v)`; `encode_metadata(m)` if a metadata was found |

`encode` and `send` are always called back to back for one piece, before the
next piece starts. Nothing is buffered across pieces or frames by the base.

### 3.4 Worked example

A frame of 1764 main ticks, filled to 1764, with metadata at 0 (`title=A`,
`foo=x`) and at 900 (`on_air=…` only, added by the output for a track
without exported keys):

| Step | Call                                  |
| ---- | ------------------------------------- |
| 1    | `encode_metadata({title=A})`          |
| 2    | `encode(frame[0,900))` then `send`    |
| 3    | `encode_metadata({})`                 |
| 4    | `encode(frame[900,1764))` then `send` |

### 3.5 Where `insert_metadata` fits

`insert_metadata` is not part of this base. It is a method of every source
that places a metadata at position 0 of the source's next non-empty frame (or
of a frame carrying a track mark); for an output it reaches the encoder only
through §3.2. Owner: the source tree
([source-contract.md](../../source/spec/source-contract.md)).

## 4. The bytes

Nothing at this level: the base hands what `encode` returned to `send`
unchanged, in piece order, and never sees bytes otherwise. `header`,
`encode_into`, `restart_offset`, `bitrate` and `stop` of an instance are never
called by the base.

## 5. Reopen, reconnect, append, file naming

Nothing at this level.

## 6. Failures

The base catches nothing. An exception from `encode_metadata`, `encode` or
`send` leaves `send_frame` at once: the remaining pieces of that frame are not
processed, and the frame is not retried. It then goes where the generic
output sends it ([output.md §6](../../outputs/spec/output.md#6-failure)),
unless the inheritor catches it in its own cycle (pipe outputs do,
[output-file.md §6](output-file.md#6-failures)).

## 7. Threads and locks

Everything here runs on the thread that animates the output's clock, inside
the output's cycle. The base takes no lock. The encoding order is a plain
mutable field set at construction.

## 8. Tests

No test targets the base alone. What protects it, by principle:

- **Pieces are cut at metadata and nowhere else.** Binding: the number of
  `encode` calls equals the number of distinct metadata positions strictly
  inside `(0, position)` plus one; no zero-length piece. Incidental: frame
  size. A harness needs a source that emits metadata at chosen offsets and a
  recording double for the three operations.
- **Metadata precedes its piece** (`metadata_first`): binding is the
  interleaving of the double's log. Covered indirectly by the in-stream
  metadata tests of each output (icecast/harbor trees).
- **Only exported keys reach the encoder**: binding is the key set seen by
  `encode_metadata`. `tests/regression/GH2756.liq` relies on an empty export
  still reaching `reopen_on_metadata` (a metadata with only `foo`).
- **Cover keys follow `export_cover_metadata`**: no test today.
