# The `output.file.hls` operator

Part A. As built. This file owns the script interface of the HLS output, its
construction, its lifecycle, the per-frame loop that drives the other
documents, and its error handling. What each step does to segments, files
and playlists is owned elsewhere and linked.

## 1. Parameters

Positional, in order:

| #   | Type                  | Meaning                                                                                                                                       |
| --- | --------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | `string`              | The output directory.                                                                                                                         |
| 2   | `[(string * format)]` | The variants: a name and an encoder format, the format carrying optional per-variant fields ([variants.md §1](variants.md#1-variant-fields)). |
| 3   | `source`              | The source.                                                                                                                                   |

Labelled, with defaults:

| Label                  | Type                                                            | Default                                   | Owner                                                                           |
| ---------------------- | --------------------------------------------------------------- | ----------------------------------------- | ------------------------------------------------------------------------------- |
| `playlist`             | `string`                                                        | `"stream.m3u8"`                           | Main playlist file name ([playlists.md §3](playlists.md#3-main-playlist)).      |
| `extra_tags`           | `[string]`                                                      | `[]`                                      | Main playlist tags, each trimmed.                                               |
| `prefix`               | `string`                                                        | `""`                                      | Prepended to URIs ([playlists.md §4](playlists.md#4-uris-and-prefix)).          |
| `main_playlist_writer` | nullable function, below                                        | `null` natively; see §1.1                 | [playlists.md §3](playlists.md#3-main-playlist)                                 |
| `segment_duration`     | `float`, seconds                                                | `10.`                                     | [segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid)               |
| `segment_name`         | `({position, extname, duration, ticks, stream_name}) -> string` | `"#{stream_name}_#{position}.#{extname}"` | [files.md §2](files.md#2-file-names)                                            |
| `segments`             | `int`                                                           | `10`                                      | Segments per media playlist ([playlists.md §2](playlists.md#2-media-playlist)). |
| `segments_overhead`    | `int?`                                                          | `5`                                       | [files.md §5](files.md#5-retention)                                             |
| `perm`                 | `int`                                                           | `0o666`                                   | [files.md §3](files.md#3-writing-a-file)                                        |
| `dir_perm`             | `int`                                                           | `0o777`                                   | [files.md §1](files.md#1-directories)                                           |
| `temp_dir`             | `string?`                                                       | `null`                                    | [files.md §3](files.md#3-writing-a-file)                                        |
| `persist_at`           | `string?`                                                       | `null`                                    | [persistence.md](persistence.md)                                                |
| `strict_persist`       | `bool`                                                          | `false`                                   | [persistence.md §4](persistence.md#4-restoring)                                 |
| `reopen_on_error`      | `(error?) -> float?`                                            | `fun (_) -> 2.`                           | §6                                                                              |

Plus the parameters common to every output: `id`, `fallible` (default
`false`), `start` (default `true`), `register_telnet` (default `true`) and
the `on_start` / `on_stop` callbacks.

`main_playlist_writer` has type
`(extra_tags : [string], prefix : string, version : int, [string.{bandwidth : int, codecs : string, video_size? : (int * int)}]) -> string?`.

### 1.1 Standard library wrapper

The standard library replaces `output.file.hls` with a wrapper that takes the
same arguments and changes one default: `main_playlist_writer` defaults to a
function that calls `hls.playlist.main` with the same arguments and returns
its result ([playlists.md §3](playlists.md#3-main-playlist)). A script that
passes `main_playlist_writer=null` gets no main playlist. Every other
behaviour is the native operator's.

## 2. Construction

Done once, when the script builds the operator, in this order. Every error
here is an invalid-value error on the named argument and stops the script.

1. **Output directory.** `~` is expanded. If the path is not an existing
   directory, it is created with every missing ancestor, each with mode
   `dir_perm` ([files.md §1](files.md#1-directories)). Failure, including a
   path that exists as a file: `Could not create or open output directory!`.
2. **Writability.** A temporary file is created and removed in the output
   directory, then in `temp_dir` if set. Failure:
   `Could not write to <output|temporary> directory "<path>": <reason>`.
3. **Device check.** If `temp_dir` is on another device than the output
   directory, a level-3 log says that each file is copied into place and
   suggests a `temp_dir` on the same file system. Construction continues.
4. **Persistence path.** A relative `persist_at` is taken relative to the
   output directory. Its parent directory is created (mode `dir_perm`) and
   probed as in step 2 (`persistence directory`).
5. **Segment duration** is rounded down to whole frames
   ([segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid)). Zero
   frames is not rejected here (finding F-SEG-1).
6. **Variants.** An empty list: `The list of streams cannot be empty`. Each
   variant is built by [variants.md §2](variants.md#2-building-a-variant).
   An unknown format: `Unsupported format`. A missing extension:
   [variants.md §3](variants.md#3-derived-properties).
7. **Playlist version** is fixed: 7 if any variant's extension is `m4s`,
   else 3. It is used by every media playlist and passed to the main
   playlist writer.
8. **Retention bound** `segments + segments_overhead`, or none when
   `segments_overhead` is `null` ([files.md §5](files.md#5-retention)).

The output's kind is `output.file`; log lines carry it.

## 3. Lifecycle

The output follows the start/stop state machine common to outputs: `idle`,
`started`, `stopped`. `start` runs on entering `started`; `stop` runs on
leaving it. Entering `started` happens at wake-up when `start=true`, by the
`start` method or command, and each time a fallible source becomes ready
again after the output went idle. Leaving it happens on the `stop` method or
command, when a fallible source ends (to `idle`), and at shutdown or sleep.

A fallible source that goes away and comes back therefore runs one `stop`
and one `start`, with the effects below.

### 3.1 Start

1. Clear the stopped flag and the reopen deadline (§6).
2. If `persist_at` is set and the file exists, restore from it
   ([persistence.md §4](persistence.md#4-restoring)).

### 3.2 Stop

1. Set the stopped flag. From here, closing a segment does not rewrite
   playlists ([files.md §4](files.md#4-closing-a-segment)).
2. Each variant's encoder is stopped and the bytes it returns are appended to
   that variant's open segment, as a zero-length chunk (§4). The whole step
   is one unit: the first exception (including a variant with no open
   segment) ends it, the remaining variants are not flushed, and the
   exception is dropped without a log.
3. With `persist_at`: each open segment is closed
   ([files.md §4](files.md#4-closing-a-segment)), then the state is written
   ([persistence.md §3](persistence.md#3-saving)).
   Without: the cleanup of [files.md §7](files.md#7-cleanup-on-stop) runs.
4. Every variant is rebuilt from its description: a new encoder, position
   1, media time 0, discontinuity count 0, init segment pending, no
   metadata, no pending tag. The per-variant lists of closed segments are
   kept as they are (finding F-LIFE-1).

## 4. Per-frame loop

Each streaming cycle while `started`, the output pulls one frame from its
source. The generic encoded-output layer cuts the frame at each metadata
position into chunks; for each non-empty chunk, in order:

1. If a metadata item starts in the chunk, it is given to every variant
   ([metadata.md §1](metadata.md#1-track-metadata)). Metadata comes first.
2. **Encode**, for each variant in list order:
   1. If the variant has no open segment, one is opened at the variant's
      media time ([segmenting.md §4](segmenting.md#4-opening-a-segment)).
   2. The next boundary is computed from the media time
      ([segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid)).
   3. While the init segment is pending, the chunk goes to `init_encode`
      ([encoder-contract.md §4](encoder-contract.md#4-init_encode)); the
      init segment it returns is written
      ([files.md §6](files.md#6-init-segments)). Once the init segment is
      known, each chunk first goes through the forced-split check
      ([segmenting.md §6](segmenting.md#6-the-target-duration-bound)), then
      to `split_encode`
      ([encoder-contract.md §5](encoder-contract.md#5-split_encode)).
3. **Send**, for each variant in list order: the media time advances by the
   chunk's length, then the pieces are applied in order
   ([segmenting.md §3](segmenting.md#3-applying-pieces)), then the open
   segment's length is set to media time minus its start.

All variants see every chunk. A chunk is a whole frame unless metadata cuts
it.

## 5. Methods and callbacks

- `insert_tag(tag)`: queues the trimmed `tag` on every variant
  ([metadata.md §3](metadata.md#3-inserted-tags)).
- `streams()`: one record per variant: `name`, `encoder` (the format),
  `video_size` (`{width, height}` or `null`), `bandwidth`, `codecs`,
  `extname`, `id3_enabled`, `replay_id3`, `extra_tags`,
  `discontinuity_count`, and `insert_tag(tag)` for that variant alone.
  Reading it computes the variant's derived properties if not yet computed
  ([variants.md §3](variants.md#3-derived-properties)).
- The start/stop methods common to outputs.
- `on_file_change({state, path})`: [files.md §8](files.md#8-file-change-events).
- With `register_telnet`, the commands common to outputs: `start`, `stop`,
  `status`, `skip`, `seek`, `metadata`, `remaining`.

## 6. Errors and reopening

Any exception raised by the per-frame loop — pulling the source, encoding,
writing or renaming a file, writing a playlist, computing a derived
property, a synchronous file-change callback — is handled here:

1. Every variant's open segment is aborted: its temporary file is closed and
   removed, no event fires, and the variant is marked to start its next
   segment with a discontinuity. The segment's position number is not
   reused.
2. `reopen_on_error` is called with the error (kind `output`). If it returns
   `null` or a negative number, the exception is raised again and the
   output fails like any output. Otherwise a level-3 log reads
   `Error while streaming: <exn>, will re-open in <d>s`, and the deadline is
   now plus `d` seconds.
3. Until the deadline, each streaming cycle skips the output entirely: the
   source is not pulled and nothing is written.

The encoders are not reset: the next segment starts with whatever bytes the
encoder produces next. With the default callback, a failure that repeats on
every frame repeats every 2 seconds without end (seen with a missing
bandwidth, [variants.md §3](variants.md#3-derived-properties), and a zero
segment duration, finding F-SEG-1).
