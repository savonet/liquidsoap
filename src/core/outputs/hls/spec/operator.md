# The `output.file.hls` operator

Part A. This file owns the script interface of the HLS output, its
construction, its lifecycle, the per-frame loop that drives the other
documents, and its error handling. What each step does to segments, files
and playlists is owned elsewhere and linked.

## 1. Parameters

Positional, in order:

| #   | Type                  | Meaning                                                        |
| --- | --------------------- | -------------------------------------------------------------- |
| 1   | `string`              | The output directory.                                          |
| 2   | `[(string * format)]` | The variants ([variants.md §1](variants.md#1-variant-fields)). |
| 3   | `source`              | The source.                                                    |

Labelled, with defaults:

| Label                  | Type                                                            | Default                                   | Owner                                                                     |
| ---------------------- | --------------------------------------------------------------- | ----------------------------------------- | ------------------------------------------------------------------------- |
| `playlist`             | `string`                                                        | `"stream.m3u8"`                           | [playlists.md §3](playlists.md#3-main-playlist)                           |
| `extra_tags`           | `[string]`                                                      | `[]`                                      | Main playlist tags, each trimmed.                                         |
| `prefix`               | `string`                                                        | `""`                                      | [playlists.md §4](playlists.md#4-uris-and-prefix)                         |
| `main_playlist_writer` | nullable function, below                                        | `null` natively; see §1.1                 | [playlists.md §3](playlists.md#3-main-playlist)                           |
| `segment_duration`     | `float`, seconds                                                | `10.`                                     | [segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration) |
| `segment_name`         | `({position, extname, duration, ticks, stream_name}) -> string` | `"#{stream_name}_#{position}.#{extname}"` | [files.md §2](files.md#2-file-names)                                      |
| `segments`             | `int`                                                           | `10`                                      | [playlists.md §2](playlists.md#2-media-playlist)                          |
| `segments_overhead`    | `int?`                                                          | `5`                                       | [files.md §5](files.md#5-retention)                                       |
| `cut_on_metadata`      | `bool?`                                                         | `null`                                    | [metadata.md §4](metadata.md#4-cuts-on-metadata-and-tags)                 |
| `perm`                 | `int`                                                           | `0o666`                                   | [files.md §3](files.md#3-writing-a-file)                                  |
| `dir_perm`             | `int`                                                           | `0o777`                                   | [files.md §1](files.md#1-directories)                                     |
| `temp_dir`             | `string?`                                                       | `null`                                    | [files.md §3](files.md#3-writing-a-file)                                  |
| `persist_at`           | `string?`                                                       | `null`                                    | [persistence.md](persistence.md)                                          |
| `strict_persist`       | `bool`                                                          | `false`                                   | [persistence.md §4](persistence.md#4-restoring)                           |
| `reopen_on_error`      | `(error?) -> float?`                                            | `fun (_) -> 2.`                           | §6                                                                        |

Plus the parameters common to every output: `id`, `fallible` (default
`false`), `start` (default `true`), `register_telnet` (default `true`) and
the `on_start` / `on_stop` callbacks.

`main_playlist_writer` has type
`(extra_tags : [string], prefix : string, version : int, [string.{bandwidth : int, codecs : string, video_size? : (int * int)}]) -> string?`.

### 1.1 Standard library wrapper

The standard library's `output.file.hls` takes the same arguments and
changes one default: `main_playlist_writer` calls `hls.playlist.main`
([playlists.md §3.1](playlists.md#31-hlsplaylistmain)). A script that
passes `main_playlist_writer=null` gets no main playlist.

## 2. Construction

Done once, when the script builds the operator. Every error is an
invalid-value error on the named argument and stops the script before
streaming starts. Reason: an error that cannot heal by itself must not
become a retry loop at runtime.

1. **Output directory.** `~` is expanded. A missing directory is created
   with its missing ancestors ([files.md §1](files.md#1-directories)).
   Failure, including a path that is a file: rejected.
2. **Writability.** A file is created and removed in the output directory,
   then in `temp_dir` if set, then in the parent of `persist_at` if set,
   which is created first. Failure: rejected, naming which directory.
3. **Devices.** A `temp_dir` on another file system than the output
   directory is reported once, with a warning that each file is then copied
   into place.
4. **Persistence path.** A relative `persist_at` is relative to the output
   directory.
5. **Numbers.** `segment_duration` shorter than one frame, `segments` below
   1, and a negative `segments_overhead` are rejected.
6. **Tags.** Every entry of `extra_tags`, and of each variant's
   `extra_tags`, is a playlist tag ([metadata.md §3](metadata.md#3-inserted-tags)),
   or the argument is rejected.
7. **Variants.** An empty list or two variants with the same name are
   rejected. Each variant is built by [variants.md §2](variants.md#2-building-a-variant).
8. **Warning**, construction continues: `segments` below 6
   ([playlists.md §2](playlists.md#2-media-playlist)).

## 3. Lifecycle

The output follows the start/stop state machine common to outputs: `idle`,
`started`, `stopped`. `start` runs on entering `started`; `stop` on leaving
it. The output's life runs from wake-up to **sleep**: the end of the
process, but also `clock.stop()`, `o.shutdown()` and the like. A stop is
**final** when it is part of going to sleep; every other stop — the `stop`
method or command, a fallible source that ends, a clock catching up — is
not.

The output's **state** is, per variant, its counters (next position
number, next init position, discontinuity count, pending inserted tags),
its retained segments and its window: what
[persistence.md §1](persistence.md#1-what-is-saved) saves. It lives in
memory from wake-up to sleep, across every run in between.

The `insert_tag` method and the `streams()` entries refer to the variants,
not to one run: a tag inserted through a value kept from before a stop
reaches the next run.

### 3.1 Start

1. At the first start since wake-up, if `persist_at` names an existing
   file: restore it
   ([persistence.md §4](persistence.md#4-restoring)).
2. A run begins ([segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration)):
   every variant gets a new encoder, media time 0, no open segment, init
   segment pending, metadata state none.
3. Unless nothing was restored and this is the first run, every variant's
   first segment is marked discontinuous.
4. The main playlist is due ([playlists.md §3](playlists.md#3-main-playlist));
   after a restore, media playlists are rewritten.

### 3.2 Stop

Every step runs for every variant whatever happened in an earlier one; a
failure is logged with the variant and the step, and the stop completes.

1. When a run is active, each variant's encoder is stopped and its pieces
   are applied ([segmenting.md §3](segmenting.md#3-applying-pieces)). Skipped during the reopen delay of §6,
   when no run is active.
2. Each open segment is closed ([files.md §4](files.md#4-closing-a-segment)),
   or dropped when empty.
3. With `persist_at`, the state is saved ([persistence.md §3](persistence.md#3-saving)).

**Sleep**, whatever the output's state (started, idle, or stopped by
hand): a stop if it is started, then, with `persist_at`, a save, and
without it the cleanup of [files.md §7](files.md#7-cleanup-at-a-final-stop).
The state is then dropped; the next wake-up starts from `persist_at` or
fresh.

A stop that is not final keeps every file and the state, so the next start
continues the same playlists with a discontinuity. Reason: a fallible
source that goes away for a moment must not remove a stream players are
following, nor make its numbering go back.

## 4. Per-frame loop

Each streaming cycle while `started`, the output pulls one frame and cuts it
at each metadata position into chunks. For each non-empty chunk, in order:

1. Metadata starting in the chunk is given to every variant
   ([metadata.md §1](metadata.md#1-track-metadata)); a cut on metadata is
   requested if due ([metadata.md §4](metadata.md#4-cuts-on-metadata-and-tags)).
2. For each variant: open a segment if none is open
   ([segmenting.md §6](segmenting.md#6-opening-a-segment)); apply the bound
   ([segmenting.md §5](segmenting.md#5-the-target-duration-bound)); encode
   (`init_encode` until the init segment is known, then `split_encode`).
3. For each variant: the media time advances by the chunk's length, then
   the pieces are applied
   ([segmenting.md §3](segmenting.md#3-applying-pieces)).

## 5. Methods and callbacks

- `insert_tag(tag)`: on every variant ([metadata.md §3](metadata.md#3-inserted-tags)).
- `streams()`: one record per variant: `name`, `encoder`, `video_size`
  (`{width, height}` or `null`), `bandwidth`, `codecs`, `extname`,
  `id3_enabled`, `replay_id3`, `extra_tags`, `discontinuity_count`, and
  `insert_tag(tag)` for that variant alone. Values are read when called
  ([variants.md §3](variants.md#3-derived-properties)).
- The start/stop methods common to outputs.
- `on_file_change({state, path})`: [files.md §8](files.md#8-file-change-events).
- With `register_telnet`, the commands common to outputs.

## 6. Errors and reopening

An exception raised while streaming — pulling the source, encoding, writing
a file or playlist, a synchronous file-change callback — ends the run:

1. Every variant's open segment is aborted
   ([segmenting.md §6](segmenting.md#6-opening-a-segment)): its temporary
   file is removed, no event fires, its discontinuity flag and tags go back
   to pending.
2. Every encoder is dropped without `stop`. Its held bytes are lost with
   the aborted segment.
3. `reopen_on_error` is called with the error (kind `output`). `null` or a
   negative number: the exception is raised again and the output fails like
   any output. Otherwise a warning reads `Error while streaming: <error>,
will re-open in <d>s`, and nothing is pulled or written for `d` seconds.
4. Then a new run begins as in §3.1 steps 2–3: new encoders, a new grid
   origin, and a discontinuity in every variant.

A file name that breaks [files.md §2](files.md#2-file-names), and a strict
restore that fails ([persistence.md §4](persistence.md#4-restoring)), make
the output fail at once, without `reopen_on_error`: neither heals by
itself.

Reason for new encoders: an encoder interrupted mid-output can only continue
with bytes that do not start at a key packet, and the segment after the
error must be decodable on its own. Closed segments, playlists and the state
are kept: published playlists name only files that exist.
