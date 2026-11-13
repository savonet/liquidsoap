# The file store

Part A. This file owns what the output does on disk: directories, file
names, how a file is written and published, closing a segment, retention
and deletion, init segments, the cleanup at a final stop, and the
file-change events. The persisted state file is
[persistence.md](persistence.md).

The output **tracks** every file it has published and not yet deleted, by
its name relative to the output directory: retained media segments, init
segments, media playlists and the main playlist. It only ever deletes files
it tracks. The tracked set is the one record of what the output serves
([serving.md §3](serving.md#3-what-is-served)); it may be read from another
thread and MUST then be read as a consistent snapshot.

## 1. Directories

Directories are created with every missing ancestor, with mode `dir_perm`
as the operating system applies it under the process umask: for the output
directory at construction, for the parent of `persist_at`, and for the
parent of every file before it is published, since a name may contain `/`.

## 2. File names

`segment_name` is called with `{position, extname, duration, ticks,
stream_name}` and returns a name relative to the output directory.

- **Media segment**: called when the segment is published, so `duration`
  (seconds) and `ticks` are its final length; `position` its position
  number ([playlists.md §2.1](playlists.md#21-media-sequence)); `extname`
  the variant's extension.
- **Init segment**: called when it is written, with `position` the
  variant's init position (§6), `extname` the container's file extension
  (`mp4` for fragmented MP4, whatever the variant's `extname`), `duration` 0
  and `ticks` 0. Reason: an init segment named with the media segments'
  extension takes a name a media segment of the next run uses.

Default: `"#{stream_name}_#{position}.#{extname}"`. The function is called
once per file and may be impure.

**Reserved names**, from construction: every media playlist
(`<variant name>.m3u8`), the main playlist, and `persist_at` with its
temporary file when they are inside the output directory.

Names are compared in their **canonical** form: the path relative to the
output directory with its empty and `.` components removed (`./a//b` is
`a/b`). A name MUST be relative and canonical, MUST NOT contain a `..`
component, MUST NOT be a reserved name, and MUST NOT be the name of a file
the output tracks. A name
that breaks this makes the output fail, without `reopen_on_error`
([operator.md §6](operator.md#6-errors-and-reopening)). Reason: the first
two would write outside the directory, the others would replace a file a
playlist names; and a naming function that returns such a name is a script
error that a retry does not heal.

## 3. Writing a file

Every segment, init segment and playlist is written the same way:

1. A temporary file is created in `temp_dir`, or in the output directory
   when `temp_dir` is `null`, with mode `perm` as the operating system
   applies it under the process umask.
2. The bytes are written. A media segment's file MAY be read back and
   truncated during a cut ([segmenting.md §4.1](segmenting.md#41-cutting)).
3. **Publish**: the parent of the final path is created (§1); the temporary
   file is renamed onto the final path. When the two are on different file
   systems, the file is first copied into a new temporary file in the final
   path's directory, which is then renamed. The file is tracked. The event
   state is `updated` if a file existed at the final path, else `created`.
4. The file-change event fires (§8).
5. **Abort** instead of publish: the temporary file is removed; no event.

The temporary file is removed on every path, errors included. A reader of
the output directory sees a final name only with its complete content.

## 4. Closing a segment

1. Publish its file (§3 steps 1–3).
2. Append it to the variant's retained segments, with its number.
3. With `persist_at`, save the state ([persistence.md §3](persistence.md#3-saving)).
4. Fire its event (§3 step 4).
5. Rewrite the variant's media playlist, then the main playlist if due
   ([playlists.md §3](playlists.md#3-main-playlist)).
6. Retention (§5).

An exception in step 1 drops the segment
([segmenting.md §6](segmenting.md#6-opening-a-segment)) and ends the run.
An exception after step 1 (a synchronous callback, a playlist write) ends
the run once the remaining steps have run: a renamed segment is always
listed. Reason: a published playlist never names a file that is gone, a
file is deleted only after the playlist that drops it is published, and
numbering never skips a published segment.

## 5. Retention

A segment that has left the window ([playlists.md §2](playlists.md#2-media-playlist))
stays on disk while either holds:

- **Availability**: the media time of the segments closed since it left the
  window is less than its own length plus the longest duration of a
  published media playlist that listed it. Reason: RFC 8216 §6.2.2 requires
  a removed segment to stay available that long, for players still working
  through an older playlist.
- **Overhead**: fewer than `segments_overhead` segments have left the
  window after it.

Otherwise it is deleted (§5.1). With `segments_overhead` `null`, nothing is
deleted while running: a segment that is no longer available stays on disk
and tracked, and is left out of the saved state
([persistence.md §3](persistence.md#3-saving)) and of init retention, which
then deletes nothing. Reason: the state stays bounded by the window however
long the output runs.

After a restore ([persistence.md §4](persistence.md#4-restoring)), a
restored segment outside the window counts as having left it at the
restore, and every restored segment as listed by a playlist as long as the
restored window. Reason: when a segment left the window and the playlists
that listed it are not saved; this assumption only lengthens retention.

Availability is counted in media time. What is not promised: a clock that
runs faster than real time, catching up, deletes segments earlier in wall
time.

An init segment is deleted once no retained segment and no open segment
refers to it.

### 5.1 Deleting a file

The `deleted` event fires, then the file is unlinked and stops being
tracked. A file already gone, or an unlink that fails, is logged as a
warning naming the path and is not an error.

## 6. Init segments

A variant's **init position** is a counter, starting at 0, that increases
by one with every init segment written. It continues across runs and is
persisted.

When the encoder hands over an init segment (`Init` or `Restart`):

- none: the following segments have no init segment;
- empty bytes: a warning naming the variant, and treated as none;
- bytes: written and published at once (§3, name by §2). The open segment,
  and every segment opened after it until the next one, refer to it.

## 7. Cleanup at a final stop

At sleep without `persist_at` ([operator.md §3.2](operator.md#32-stop)),
whatever the output's state:

1. Every open segment is aborted (§3 step 5).
2. The main playlist, then every media playlist, is deleted (§5.1).
3. Every retained segment, then every init segment, is deleted.

Afterwards the output tracks no file. Files the output never published are
not touched.

What is not promised: RFC 8216 §6.2.2 asks that the segments of a removed
presentation stay available for the playlist's duration. Sleep is usually
the end of the process; files go at once, and the playlists go first so
that no player is directed to a missing segment.

## 8. File-change events

`on_file_change` callbacks receive `{state, path}`, with `path` the output
directory joined with the file's name, the same for every event of a file:

| State     | When                                  |
| --------- | ------------------------------------- |
| `created` | after the rename of §3, no prior file |
| `updated` | after the rename of §3, over a file   |
| `deleted` | before the unlink of §5.1             |

Events fire for media segments, init segments, media playlists and the main
playlist; never for temporary files or the persisted state. While the
output tracks a file: one `created` or `updated` first (`updated` when it
replaced a file left by an earlier process), then any `updated`, then at
most one `deleted`. The file is complete when `created` or `updated` fires.
Callbacks registered with `synchronous=true` run in the streaming cycle, in
registration order; an exception they raise ends the run as §4 says.
