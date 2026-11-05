# The file store

Part A. As built. This file owns what the output does on disk: directories,
file names, how a file is written and published, closing a segment,
retention and deletion, init segments, the cleanup at `stop`, and the
file-change events. The persisted state file is
[persistence.md](persistence.md).

## 1. Directories

Directories are created with every missing ancestor, each with mode
`dir_perm` as the operating system applies it (the process umask applies).
This happens for the output directory at construction
([operator.md §2](operator.md#2-construction)), for the parent of
`persist_at`, and for the parent of every file before it is published, since
a name may contain `/`.

## 2. File names

`segment_name` is called with a record `{position, extname, duration,
ticks, stream_name}` and returns a name relative to the output directory.

- **Media segment**: called when the segment is **published** (§3), so
  `duration` (seconds) and `ticks` are its final length; `position` is its
  position number ([segmenting.md §8](segmenting.md#8-position-numbers)),
  `extname` the variant's extension.
- **Init segment**: called when it is written, with `position` the variant's
  init position (§6), `extname` `mp4` for an `m4s` variant and the
  variant's extension otherwise, `duration` 0 and `ticks` 0.

Default: `"#{stream_name}_#{position}.#{extname}"`. The function is called
once per file and may be impure (tests use the time of day).

Media playlists are `<variant name>.m3u8`; the main playlist is the
`playlist` parameter. Neither goes through `segment_name`.

## 3. Writing a file

Every segment, init segment and playlist is written the same way:

1. A temporary file `liq<random>tmp` is created in `temp_dir`, or in the
   output directory when `temp_dir` is `null`. Its mode is set to exactly
   `perm`, without the umask (finding F-FS-1: files are `0666` under umask
   `022`, checked).
2. Bytes are written to it. A media segment's file may be read back and
   truncated during a cut ([segmenting.md §5.1](segmenting.md#51-cutting));
   the read is preceded by an `fsync`.
3. **Publish**: the final path is the output directory joined with the
   name; its parent directory is created (§1); the event state is `updated`
   if a file exists there, else `created`; the temporary file is renamed onto
   the final path. If the rename fails because the two are on different
   devices, the file is copied into a second temporary file in the final
   path's directory (mode `perm`), which is then renamed onto the final path.
   The first temporary file is removed in every case.
4. The file-change event fires (§8).
5. **Abort** instead of publish: the temporary file is closed and removed;
   no event.

A reader of the output directory therefore sees a final name only with its
complete content. Temporary files sit in the output directory, under their
own names, while being written, when `temp_dir` is `null`.

## 4. Closing a segment

1. Publish its file (§3). The segment's recorded file name is the full final
   path.
2. Append it to the variant's list of closed segments.
3. Retention (§5).
4. The variant has no open segment.
5. Unless the output is stopping: rewrite the variant's media playlist, then
   write the main playlist if due
   ([playlists.md §3](playlists.md#3-main-playlist)).

An exception in step 1 leaves the segment out of the list and propagates
([operator.md §6](operator.md#6-errors-and-reopening)).

## 5. Retention

With `segments_overhead` `null`, nothing is ever deleted while running.
Otherwise, with bound `B = segments + segments_overhead`, after each append:
if the list holds `B` or more segments, the oldest is removed from it and:

1. its file is deleted (§5.1);
2. its init segment is deleted if a file by its recorded name exists and no
   segment left in the list shares it. The recorded init name is the
   relative name, tested and deleted relative to the process's working
   directory, so in practice init segments are never deleted (finding
   F-FS-2, checked).

So `B − 1` closed segments stay on disk, plus the open one (finding F-FS-3,
checked: `segments=3`, `segments_overhead=1` keeps three closed segments).
Deletion runs before the playlist rewrite of §4 step 5. With
`segments_overhead` 1, the deleted segment is still named by the published
media playlist until that rewrite.

### 5.1 Deleting a file

The `deleted` event fires first, with the path as recorded, then the file is
unlinked. A failure is logged at level 3, `Could not remove file <path>:
<reason>`, and is not an error.

## 6. Init segments

A variant's **init position** starts at 0, increases by one at each
container restart ([segmenting.md §7](segmenting.md#7-container-restart)),
and is set by a resume ([persistence.md §4](persistence.md#4-restoring)).

When the encoder hands over an init segment (first `Init`, or a `Restart`):

- none: the variant has no init segment;
- empty bytes: level-3 log `Stream <name>: empty init segment ignored.`, no
  init segment;
- bytes: written and published as a file (§3, name by §2) at once. The open
  segment, and every segment opened after it until the next restart, record
  that init segment's name as returned by `segment_name`.

## 7. Cleanup on stop

At `stop` without `persist_at` ([operator.md §3.2](operator.md#32-stop)):

1. Every closed segment in the lists is deleted (§5.1).
2. For each variant with an open segment: the segment is **published**
   (§3, so a `created` event fires for it), its init segment is deleted if
   its relative name exists in the working directory, and the segment would
   be deleted by its recorded name, which a published-but-not-closed
   segment does not have. The open segment's file stays (finding F-FS-4,
   checked).
3. Each media playlist and the main playlist are deleted by their bare
   names, relative to the process's working directory. The output's own
   playlists stay, and same-named files in the working directory are
   deleted (finding F-FS-5, checked).
4. The main playlist is due again.

The init segments of closed segments are not deleted by the cleanup. The
lists of closed segments are not emptied (finding F-LIFE-1).

## 8. File-change events

`on_file_change` callbacks receive `{state, path}`:

| State     | When                                  | Path                                                                            |
| --------- | ------------------------------------- | ------------------------------------------------------------------------------- |
| `created` | after the rename of §3, no prior file | output directory joined with the name                                           |
| `updated` | after the rename of §3, over a file   | same                                                                            |
| `deleted` | before the unlink of §5.1             | as recorded: full for media segments; bare name for init segments and playlists |

Events fire for media segments, init segments, media playlists and the main
playlist; never for temporary files or the persisted state. Callbacks
registered with `synchronous=true` run in the streaming cycle, in
registration order, and an exception they raise takes the error path of
[operator.md §6](operator.md#6-errors-and-reopening). A `deleted` event is
sent even when the unlink then fails.
