# Known complexity: language notes

Part B. Difficulties from the history that only make sense with the OCaml
code or its bindings in view.

## Area C: Native encoders and Ogg

- Callback-based codec libraries (libFLAC) call into the managed runtime
  during init, encode and finish; buffers registered with the GC were moved
  or collected: segfaults after hours (#3495, #4133, #4286; fixes in the
  binding's global-root handling).
- A per-call worst-case output buffer on the C stack (LAME, ≈144 KB)
  overflows musl's 128 KB default thread stack: segfault only on Alpine,
  first misattributed to OCaml 5 (#3936, #5322).
- An `if … then a; b` without parentheses made `%external` restart on every
  metadata (0f4eec6cc, #789).
- `Option.get` on the video size evaluated outside the video branch made an
  audio-only `%external` fail (04ca200a2, #3020).
- Process end detected by a zero read on a pipe that a grandchild holds open
  never arrives; an explicit exit signal was added (1d0e4b88c, #3029).
- The Ogg page-fill threshold is compiled in only under a C preprocessor
  flag that no build rule defines after vendoring (d772ceee5; finding D5).
- Unexplained, not entered: gradual volume loss over hours with FDK-AAC
  (#2638) and earlier with another encoder (#211); closed without a cause.
- NDI: no history beyond its addition (356d3f0e7, #4181); its timecode drift
  is a pass-1 finding only.

## Area F: The seam, file and process outputs

- Instance operations were mutable record fields (`header`), captured by value
  in the locking wrapper; a field read at wrap time froze it (318cba9f7).
- `Option.get` on the encoder or channel slot was the crash site of most
  KF-11 entries; the slot became a three-state variant (e87290d32).
- FFmpeg format options are a `Hashtbl` inside an otherwise immutable record:
  the file rewrite mutated the script's value until it copied (1c17b384a).
- OCaml `Lazy` forced from several threads is unspecified; deferred format
  values moved to a mutexed lazy (39ac9e13e). The closures inside make
  `compare` on formats raise (seam D4).
- `Unix.Unix_error` vs `Sys_error` were handled by different catch sites,
  which mis-kinded directory errors (2731e5a0d).
- The encoder API changed from `(frame, offset, length)` to pre-sliced frames
  (8af9836af) and `insert_metadata` was renamed `encode_metadata`
  (e01202552); history before those commits uses the old names.

## Area N: Network outputs and the `%ffmpeg` destination

- The vendored protocol library's non-chunked write uses `length` as an end
  index when given an offset (finding net-callers D1, latent).
- A per-connection header table filled with `Hashtbl.add` grew on every
  attempt (e4c6ffc2f, #490).
- The retry time is set from `Unix.gettimeofday` and compared with
  `Unix.time` (finding D4).
- `%argsof` let a wrapper's own `icy_id` be shadowed by the wrapped
  operator's (#2819).
- `SO_SNDTIMEO`/`SO_RCVTIMEO` behave differently across platforms
  (7692153b5); Windows reports a failed non-blocking connect in the
  exception set of `select` (a7754004a).
