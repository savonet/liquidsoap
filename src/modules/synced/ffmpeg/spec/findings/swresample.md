# swresample — findings

Each finding carries its verification verdict. Sections **Gaps** and **To
verify** stay **read only** except where a verdict says otherwise. Paths are relative to
`swresample/`.

## Defects

### `flush` does not put libswresample in flush mode

`swresample_stubs.c:474-489`, `:417-418`. `flush` calls the shared `convert`
helper, which passes `swr->in.data` as the input pointer with a count of 0.
For every non-frame input kind that pointer is the context's own table and is
never null. libswresample enters flush mode on a null input pointer (see To
verify). The `.mli` says `flush` "flushes the last remaining data"
(`swresample.mli:88`). With a frame input kind the pointer is null only before
the first `convert`, and afterwards is the stale `extended_data` of the last
input frame.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Mono double 44100 -> 48000, 44100 samples in ten `convert` calls: 47983 samples out; three `flush` calls returned 0, 0, 0 (17 missing). Same with frame input (flush before the first `convert`: 0; after: 0). C control on the same library: `swr_convert` with a non-null input and count 0 returns 0, with a null input returns 17, total 48000. `swr_convert` at `n7.1.5` and `n9.0.2` calls `resampler->flush` only under `if (!in_arg)`. Without rate conversion there is no tail to lose.

### `offset` is wrong for interleaved bytes

`swresample_stubs.c:109-125`. The sample count subtracts `offset`, the buffer
is sized for that count, and the copy still transfers the whole `str_len`
bytes starting at `offset * bytes_per_frame`. A non-zero offset reads
`offset * bytes_per_frame` bytes past the end of the OCaml bytes and writes
the same amount past the samples the scratch buffer was sized for.

**narrowed** — the samples returned are the right ones (run: stereo doubles 1..16, `~offset:2` returned 5..16); the defect is the copy length only: `str_len` bytes are read from `offset * bytes_per_frame`, so the read ends that many bytes past the OCaml bytes and the write ends that many bytes past the samples the buffer was sized for (second read; the overrun was not instrumented).

### `offset` is wrong for interleaved float arrays

`swresample_stubs.c:158-176`. The loop runs over all `linesize` elements and
reads element `i + offset`: the shift is `offset` elements instead of
`offset * nb_channels`, the read runs `offset` elements past the array, and
`linesize` doubles are written into a buffer sized for
`(linesize / nb_channels - offset) * nb_channels`.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Stereo 1..16: `~offset:2` returned 3..14 (wanted 5..16), `~offset:1` returned 2..15, which swaps left and right. The over-read and over-write of `offset` elements follow from the same loop (second read).

### `offset > 0` always fails for planar bytes

`swresample_stubs.c:131-149`. `str_len` is the full length of channel 0; each
channel is required to satisfy `str_len == length - offset * bytes_per_sample`.
Channel 0 fails that test whenever the offset is positive.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Two 64-byte double planes: offset 0 converts, `~offset:2` raises `Failure "Swresample failed to convert channel 0's 64 bytes : 64 bytes were expected"`.

### `offset > 0` always fails for planar bigarrays

`swresample_stubs.c:226-238`. `nb_samples` is `dim - offset`; each channel is
required to satisfy `nb_samples == dim`. Channel 0 fails whenever the offset
is positive.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Two 8-element planes: offset 0 converts, `~offset:2` raises `Failure "Swresample failed to convert channel 0's 8 bytes : 6 bytes were expected"`. Line `:240` is therefore reached only with offset 0.

### Bigarray offsets are applied in bytes

`swresample_stubs.c:218`, `:240`. The offset is added to the `void *` data
pointer without multiplying by the sample size. It is correct only for 8-bit
formats.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Interleaved mono, input 1..16, `~offset:2`: u8 returned 3..16 (correct), s16 returned 2..15 (one sample), f64 returned 14 denormals (a read 2 bytes into the first double). Only `:218` is observable: the planar reader at `:240` rejects every positive offset first.

### The module's sample format silently overrides the argument

`swresample.ml:292-305` against `swresample.mli:32-42`, `:53-79`. The `.mli`
says the module's format is used "if a sample format parameter is not
provided", and that `from_codec` uses "the audio codec properties as input
format". The code takes the module's format whenever it is defined and looks
at the argument only for `Bytes` and `Frame`. `from_codec` with `S16Frame`
and a codec in `fltp` creates an S16 context; every decoded frame is then
rejected by the format check in `convert`.

**confirmed (second read)** — looked for a comparison between `I.sf` / `O.sf` and the argument in `create` and in the stub; there is none, and the first match arm discards the argument.

### `create` leaks on every failure after the record is allocated

`swresample_stubs.c:524-620`, `:629-641`. `swresample_set_context` raises
directly from `swr_alloc`, each `av_opt_set_*`, `av_channel_layout_copy` and
`swr_init`. Nothing frees the `swr_t`, the `SwrContext` or the layout copy,
and no OCaml value owns them yet. The `if (!ctx)` cleanup at `:638-641` is
unreachable: the function returns a non-null pointer or raises.

**confirmed (second read)** — looked for an owner of `swr` before the raise: the custom block is allocated at `:694`, after `swresample_create` returns, and `swresample_set_context` has no cleanup path.

### The output channel layout copy is never uninitialised

`swresample_stubs.c:563` against `:491-512`. `swresample_free` frees the
record without `av_channel_layout_uninit(&swr->out_ch_layout)`. This leaks
when the layout owns heap memory (see To verify).

**confirmed (second read)** — `av_channel_layout_copy` at `n8.1.3` allocates `u.map` only for `AV_CHANNEL_ORDER_CUSTOM`, so the leak is limited to custom-order output layouts; `swresample_free` has no uninit and `av_free(swr)` drops the pointer.

### The stubs include libavformat's header, which the library neither requires nor links

**checked** — preprocessing the stub file with only libswresample's and
libavcodec's headers leaves `LIBAVFORMAT_VERSION_INT` undefined; the file
compiles because it includes `libavformat/avformat.h`
(`swresample_stubs.c:20`). Detection for this library asks pkg-config for
`libswresample` only, so the header is found only when libavformat's headers
share an include directory with libswresample's. The one use is the dead
version branch on the frame channel count (`swresample_stubs.c:82-88`): with
the header absent, an undefined macro reads as 0 in `#elif`, the
`frame->channels` branch is selected, and the file fails to compile against
any FFmpeg that passes detection.

## Asymmetries

### Only one reader handles `offset` correctly

`swresample_stubs.c:178-207`. The planar float array reader shifts the read
index, bounds the loop by the reduced count and compares full lengths. Its
five siblings each get at least one of these wrong (see Defects); the frame
reader rejects any offset.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Planar float arrays, `~offset:2`: 3 103 4 104 ... 8 108, as wanted. Interleaved bytes and u8 bigarrays also return the right samples; the bytes reader still over-reads.

### Copying readers copy more than `length`

`swresample_stubs.c:456-462`. `length` only lowers the count given to
`swr_convert`. The bytes and float readers have already copied the whole
input.

**confirmed (second read)** — `length` is read at `:456`, after `get_in` has returned, and no reader takes it.

### Output count passed to `swr_convert` differs by kind

`swresample_stubs.c:417`. The count is `out.nb_samples`: the high-water
capacity for scratch kinds, the exact estimate for bigarray and frame kinds.

**confirmed (second read)** — `alloc_out_data` only grows `out.nb_samples`; the bigarray and frame allocators assign it on every call.

## Gaps

### Nothing ties the vector kind to the planarity of the format

`swresample_stubs.c:434-441`, `:646`, `:653`. The `Bytes` module (`Str`,
format `` `None ``) accepts any explicit format, planar included. The planar
flag then follows the format: `convert` compares `Wosize_val` of a `bytes`
value with the channel count, the interleaved reader copies everything into
plane 0 of a planar allocation, and the interleaved finisher reads
`ret * channels * bytes` from plane 0.

### `offset` and `length` are not checked for sign

`swresample_stubs.c:447-462`. A negative offset enlarges the sample count and
moves reads before the start of the input. A negative length reaches
`swr_get_out_samples` and `swr_convert`.

### `swr_get_out_samples` result is unchecked

`swresample_stubs.c:466`, `:483`. A negative result goes to the allocators as
a sample count.

### Pointer table allocations are unchecked

`swresample_stubs.c:644-645`, `:651-652`. A null `av_calloc` result is
dereferenced on first use.

### An input shorter than one frame on a fresh context

`swresample_stubs.c:117-122`, `:166-173`. With a zero capacity and a zero
sample count no buffer is allocated, yet the bytes reader copies `str_len`
bytes and the float reader `linesize` doubles to the null slot 0.

### `sample format = None` leaves the recorded format at zero

`swresample_stubs.c:544-550`, `:571-576`. An explicit `` `None `` passed to
`Bytes` or `Frame` skips both the AVOption and the record field, which stays 0
(`AV_SAMPLE_FMT_U8`). The code relies on `swr_init` failing.

### Options after the third are dropped

`swresample_stubs.c:522`, `:684-687`. The list type allows any length and
repeated categories; only the first three elements are applied, and a value
found in no table is ignored without error.

### No mutual exclusion on a context

`swresample_stubs.c:416-419`. The runtime lock is released during
`swr_convert` while the context's tables and scratch buffers are in use. A
second thread calling `convert` on the same context can free and replace
those buffers (`alloc_data`).

### Frame input: only channel count and format are compared

`swresample_stubs.c:80-107`. Sample rate and channel layout are not.

### Dead version branches

`swresample_stubs.c:82-88`. The two older branches test `LIBAVCODEC` and
`LIBAVFORMAT` versions for a frame field and cannot compile together with the
unconditional `AVChannelLayout` use elsewhere in the file.

### Contexts report no size to the garbage collector

`swresample_stubs.c:694`. `caml_alloc_custom(..., 0, 1)`: scratch buffers and
the `SwrContext` exert no collection pressure, and collection is the only
release.

## API gaps

- **No working drain.** `flush` is the only end-of-stream operation and does
  not flush (first Defect). The resampler tail cannot be retrieved.
  **checked** — see the first Defect: 17 of 48000 samples are never returned.
- **`convert ~offset` on a frame kind can only fail** for any non-zero offset
  (`swresample_stubs.c:90-91`), while `?offset` is part of the one `convert`
  signature shared by all kinds.
  **confirmed (second read)** — `:90-91` is unconditional and precedes every other check.
- **Two sources for the sample format.** Typed modules and the
  `?in_sample_format` / `?out_sample_format` arguments both state it; the
  signature accepts both and the argument is discarded. The codec wrappers
  always pass the argument.
  **confirmed (second read)** — same check as the Defect above.
- **`Bytes` admits planar formats** that its `bytes` type cannot represent.
  **confirmed (second read)** — no planarity test on the argument in `swresample.ml` or between `:643` and `:659`.
- **`AudioData` is an open signature.** `Make` accepts any user module; the
  stubs trust `vk` and `sf` to describe `t` with no check
  (`swresample.mli:17-22`). A mismatched module is memory-unsafe.
  **confirmed (second read)** — `vector_kind` and `AudioData` are exported by the `.mli`; `(**/**)` hides `vector_kind` from the documentation only.
- **`length` and `offset` units are undocumented** in the `.mli`; the code
  means samples per channel.
  **confirmed (second read)** — the `convert` comment at `swresample.mli:80-85` names neither argument.

## To verify

- `swr_convert` enters flush mode only when the input pointer is null; with a
  non-null pointer and a zero count it returns already-buffered output only.
- `av_samples_alloc` with alignment 0 rounds the sample count up to a
  multiple of 32, which hides small overruns (trailing partial frame, small
  offsets) from tools and from crashes.
- `av_channel_layout_copy` allocates only for custom-order layouts.
- `av_frame_get_buffer` returns `EINVAL` for `nb_samples == 0`. If so, a
  frame-output `convert` or `flush` whose estimate is 0 raises instead of
  returning an empty frame (`swresample_stubs.c:260-276`).
- `caml_alloc(0, Double_array_tag)` returns the zero-size atom with the
  double-array tag, not the atom OCaml uses for `[||]`; structural comparison
  of an empty float-array result with `[||]` may then be false
  (`swresample_stubs.c:351`, `:367`).
- `swr_init` rejects a context whose `in_sample_fmt` / `out_sample_fmt` or
  sample rates were left unset.
- `swr_get_out_samples` is an upper bound for the next `swr_convert` call
  with that input count.

Settled in verification: the first item holds (`swr_convert` at `n7.1.5`, `n9.0.2`, and the C control above); the third holds (`av_channel_layout_copy` at `n8.1.3`).

## Refuted

### Allocation inside `Store_field` arguments

`swresample_stubs.c:311-312`, `:340`, `:366-367`. `Store_field(*out_vect, i,
caml_ba_alloc(...))` and the `caml_alloc_string` / `caml_alloc` equivalents
allocate inside the macro argument. `avutil/avutil_stubs.c:130-132` documents
the project's own rule against this: the destination address may be computed
before the allocation moves the block.

**refuted** — `Store_field` in the installed OCaml 5.5.0 `caml/memory.h` first assigns `value caml__temp_val = (val);` as its own statement and only then evaluates `&Field((block), offset)`, so the allocation completes before the destination is read; here `block` is `*out_vect` / `*tmp`, a registered root re-read after the allocation. What remains is a departure from the house rule stated in `avutil/avutil_stubs.c:130-132`, whose stated reason does not apply to this macro.
