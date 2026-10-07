# `av` — findings

Every entry under **Defects** and **API gaps** carries a verdict from a
second, adversarial pass; so do G1, G4, G14, A3 and V1, V2, V4, V5. Entries
without a verdict are still **read only**. Paths are relative to the
bindings root.

Runs marked **checked** used a throwaway program linked against the bindings
built from this tree and libavformat 63 (`N-126774-g6242bd002d`), one case
per process. FFmpeg source was read at `n7.1.5`, `n8.1.3` and `n9.0.2`.

## Defects

### D1. Decoder open failure leaves a dangling stream-table entry

`av/av_stubs.c:1123-1141` (with `:1084`, `:1388-1391`, `:122-127`).
`allocate_stream_context` stores the new entry in `av->streams[index]`.
When `avcodec_parameters_to_context` or the codec open fails,
`open_stream_index` does `av_free(stream)` and raises. The table slot still
points at the freed record and the codec context is leaked. The next
`read_input` for that stream finds a non-null slot and uses freed memory;
`close_av` frees it a second time. Reachable from OCaml whenever a decoder
fails to open for a frame-selected stream.

**checked.** `Av.open_input ~configure_audio_stream:(fun _ -> { codec = Some flac; opts = None })` on a PCM wav, then `read_input ~audio_frame:[s]`: the first read raised `Error Invalid argument` ("Codec type or id mismatches"), the second read segfaulted (exit 139); with `close` directly after the failed read, `close` segfaulted (exit 139). Control without the forced decoder: three frames, clean close, exit 0. The same stale slot is left when `avcodec_alloc_context3` fails (`:1089-1093`): the slot keeps a record with a null codec context and the next read dereferences it (read only, allocation failure).

### D2. `open_output`: interrupt root left on freed memory

`av/av_stubs.c:1648-1665`, `:1683-1693`. The interrupt closure is registered
as a global root inside `av` before `avformat_alloc_output_context2`. When
that call fails (for example no format given and none guessable from the
file name), `av` is freed without removing the root. The same holds for the
failure of `av_opt_set_dict` on `priv_data`. The GC then scans a root
address inside freed memory. Reachable with `?interrupt` and a bad file
name.

**checked.** 3000 failed `Av.open_output ~interrupt "x.nosuchext"` interleaved with malloc churn: segfault (exit 139) at the first `Gc.full_major`, after 500 opens, on both runs. The same loop without `~interrupt` completed on both runs (exit 0).

### D3. `open_output`: double free when a format option is rejected

`av/av_stubs.c:1669-1681`. On failure of `av_opt_set_dict(format_context)`
the code runs `av_free(av)` at `:1673` and again at `:1678`. Reachable with
an option the format context knows but whose value it rejects.

**checked.** `Av.open_output ~opts:{avoid_negative_ts = "bogus"} "d3.wav"` printed `free(): double free detected in tcache 2` and aborted (exit 134).

### D4. `open_output`: every failure path leaks the format context

`av/av_stubs.c:1669-1728`. After `avformat_alloc_output_context2` succeeded,
no failure branch calls `avformat_free_context`. The custom-I/O branch at
`:1698-1706` and the `avio_open2` branch call `av_dict_free(options)` twice
(harmless) and never free the context.

**confirmed (second read).** Looked for `avformat_free_context` or `close_av` on each failure branch and in the three callers: there is none, and `av` is not yet in a custom block, so no finaliser runs either.

### D5. `open_av` never frees the container record on failure

`av/av_stubs.c:733-744`. The `fail:` label frees the URL, the dictionary,
the packet, the frame and the format context, not `av` itself. Every failed
`open_input` / `open_input_stream` leaks the record.

**confirmed (second read).** `avformat_open_input` frees the format context and nulls the pointer on failure (`demux.c` n8.1.3 `:367-373`), so the later `avformat_close_input` is a no-op and `av` is the only thing left; `open_av` raises, so no caller can free it.

### D6. Probing failure leaks the option dictionary

`av/av_stubs.c:756-761`, `:914-918`. When `avformat_find_stream_info` fails,
the container is torn down and the error raised; the remaining entries of
`options` are not freed.

**confirmed (second read).** `avformat_open_input` leaves the unconsumed entries in `*options` (`demux.c` n8.1.3 `:358-359`) and `close_av` never touches the dictionary.

### D7. A raising configure callback leaks the half-opened input

`av/av_stubs.c:835-836`. `caml_callback` (not `_exn`) is used between
`avformat_open_input` and the creation of the custom block. An exception
(or a failure in `value_of_codec_parameters_copy`) unwinds past a container
that no OCaml value references: format context, work packet and frame,
`stream_decoders`, `stream_opts`, `options` and the interrupt root are all
left behind. The interrupt root then keeps its closure alive for ever.

**confirmed (second read).** The container enters a custom block only at `:922-923`; nothing between `:793` and there traps an exception, and `Gc.finalise` is attached in `av.ml` after the stub returns.

### D8. `get_container_stream_time_base` skips the closed check

`av/av_stubs.c:253-269`. It uses `Av_base_val`, then dereferences
`av->format_context`, which is null after close. Every sibling uses
`Av_val`. A call on a closed container is a null dereference.

**checked.** `open_output`, `new_audio_stream`, `close`, then `get_container_stream_time_base ~index:0`: segfault (exit 139).

### D9. `tell` truncates the position to a C `int`

`av/av_stubs.c:2497-2512`. `avio_tell` returns `int64_t`; the result is
stored in `int ret`. Past 2 GiB the value wraps; a negative wrap is raised
as an error code. The comment at `:2502` also says the call reaches the
OCaml seek callback (see V4).

**confirmed (second read).** `avio_tell` is `avio_seek(s, 0, SEEK_CUR)` returning `int64_t` (`avio.h`); `Val_int(ret)` is applied to the truncated `int`. The comment is wrong too: see V4.

### D10. `write_packet` reads an OCaml value with the runtime lock released

`av/av_stubs.c:2175-2192`. `rational_of_value(_time_base)` is evaluated
after `caml_release_runtime_system()`. Another thread can run the GC
meanwhile. `write_frame` and the subtitle path read everything they need
before releasing.

**confirmed (second read).** `rational_of_value` reads two fields of `_time_base` at `:2191`, after the release at `:2175`; a minor collection on another thread can turn the young block into a forwarding header under the read. `write_frame` and `write_subtitle_frame` take only C values across the release.

### D11. `codec_attr` discards a successfully parsed HEVC SPS

`av/av_stubs.c:2657-2661`. After reading profile and level from the SPS the
code does `goto fail`, which returns `None`. The source it follows
(libavformat `hlsenc.c`) breaks out of the loop to format the string. As
built, an HEVC stream with Annex-B extradata containing an SPS always
yields `None`.

**confirmed (second read).** `hlsenc.c` at n7.1.5 `:403-413` uses `break` after reading profile and level; the binding's `goto fail` at `:2661` returns `None`. FFmpeg 8 and 9 moved this code to `ff_make_codec_str`.

### D12. `.mli` says a raising I/O callback gives `` `Unknown``

`av/av.mli:76-78`, `:266-267`; `av/av_stubs.c:388-391,427-430,463-466`;
`avutil/avutil_stubs.c:40-99`. The callbacks return `AVERROR_EXTERNAL`,
which the error mapping has no case for, so an unchanged code surfaces as
`` `Other n``, not `` `Unknown``.

**confirmed (second read).** `ocaml_avutil_raise_error` has no `AVERROR_EXTERNAL` case and `Avutil.error` has no constructor for it; `` `Unknown`` is produced for `AVERROR_UNKNOWN` only.

### D13. `.mli` says `read_input` reads "all streams otherwise"

`av/av.mli:198-201`; `av/av_stubs.c:1376-1405`. With no stream selected,
every packet takes the unhandled branch; the call consumes the whole input
and raises ``Error `Eof``. Nothing is ever returned.

**confirmed (second read).** With both arrays empty the packet falls to `:1395-1404` every time; at end of file `drain_decoders` iterates an empty `_frame` and `:1355` raises `` `Eof``.

### D14. Decoder-conflict comment contradicts the code

`av/av_stubs.c:805-807` vs `:875-900`. The comment says conflicting
preferences "cancel each other out"; the code keeps the first codec, logs a
warning and still forces it for probing.

**confirmed (second read).** The `*_conflict` flags only gate the warning; the assignments at `:881`, `:890`, `:899` are unconditional once an override exists.

### D15. `av_freep(buffer)` on the I/O buffer

`av/av_stubs.c:584`. `av_freep` takes the address of a pointer; it is given
the buffer itself. On `avio_alloc_context` failure this frees whatever the
first bytes of the buffer happen to hold and leaks the buffer.

**confirmed (second read).** `av_freep` reads a pointer from the address it is given: here the first eight bytes of a fresh `av_malloc` block, which it then frees. Allocation-failure path only.

### D16. `new_stream`: dangling table entry when `avformat_new_stream` fails

`av/av_stubs.c:1905-1915`. The entry is already stored in `av->streams` and
`nb_allocated_streams` counts it; `free_stream(stream)` frees it without
clearing the slot, so `close_av` frees it again. Allocation-failure path
only.

**confirmed (second read).** `close_av` iterates `nb_allocated_streams` (`:124`), which counts the freed slot. A later `new_stream` call resets the slot at `:1901`, so the double free needs `close` (or GC cleanup) to come next.

### D17. `.mli` comment mismatches

`av/av.mli:437-448`: `write_subtitle_frame` documents `?on_keyframe`; the
signature has none and the stub ignores the closure for subtitles
(`av_stubs.c:2466-2467`). `av/av.mli:392-398`: `new_data_stream` documents
`opts`; there is none. `av/av.mli:150-151`: comment names `Av.get_codec`
for `get_codec_params`.

**confirmed (second read).** All three comments read against their signatures in `av.mli`.

## Asymmetries

### A1. Interrupt callback: format context for inputs, I/O context only for outputs

`av/av_stubs.c:716-722` vs `:1629,1648-1653,1713`. Inputs set
`format_context->interrupt_callback`. Outputs only pass the callback to
`avio_open2`. I/O that a muxer opens itself through the format context
(segmenting muxers, nested protocols) is not interruptible on outputs.

### A2. `get_input_duration` maps 0 to `None`, `get_duration` does not

`av/av.ml:150-151` vs `:225-226`.

### A3. `read_input` checks stream/container identity, `seek` does not

`av/av.ml:251-261` vs `av/av_stubs.c:1475-1482`. A stream of another
container given to `seek` indexes this container's streams unchecked.

**confirmed (second read).** `:1476-1481` index `format_context->streams` with the stream's index and no bound check; `av.ml` `seek` passes the stream through untouched.

### A4. `open_input` custom inputs get neither preferred decoders nor interrupt

`av/av.ml:141-144`, `av/av_stubs.c:938-976`. `open_input_stream` cannot
choose decoders or pass per-stream options; `open_input` can.

### A5. Explicit close vs GC cleanup of outputs

`av/av_stubs.c:2517-2558`. `close` drains encoders and writes the trailer;
the `Gc.finalise` path does neither. An output dropped without `close` is
truncated silently.

### A6. Header check on stream mutation

`av/av_stubs.c:1891` and `:1863` refuse after the header is written;
`ocaml_av_initialize_stream_copy` (`:1999-2014`), `set_time_base` (`:283`)
and `set_avg_frame_rate` (`:234`) do not.

## Gaps

### G1. A failed encoding-stream creation leaves the stream in the container

`av/av_stubs.c:1948-1963`, `:1970-1985`, `:2044-2046`, `:2086-2103`. When the
encoder fails to open (bad option, unsupported parameters) the `AVStream`
and the table entry with an unopened codec context stay. A later `close`
flushes that entry: it writes the header with a half-initialised stream and
`avcodec_send_frame` fails, so `close` raises before the trailer and before
release. Nothing removes the stream.

**checked.** `new_audio_stream` with `pcm_s16le` and sample format `` `Dbl`` raised `Error Invalid argument`; `close` then raised `Error Invalid argument` on each of two calls, for `wav` ("muxer does not support any stream of type unknown"), `nut` ("No codec tag defined for stream 0") and `mp4` ("Could not find tag for codec none"). The failure is in `avformat_write_header`, so any write to a healthy stream of the same container fails the same way: the output is unusable from the failed creation on.

### G2. `close` that raises leaves the container open; trailer errors are dropped

`av/av_stubs.c:2521-2546`. An encoder-flush error skips the remaining
streams, the trailer and `close_av`. Conversely the result of
`av_write_trailer` (`:2541`) and of `avio_closep` (`:141`) is ignored, so a
failed final write (including a raising custom write callback) is not
reported.

### G3. Custom read callback result is not bounded

`av/av_stubs.c:386-398`. `memcpy(buf, buffer, Int_val(res))` trusts the
closure's return value. A value above the requested length overflows
FFmpeg's buffer; above 32768 it also reads past the OCaml buffer.

### G4. A short count from the custom write closure is taken as a full write

`av/av_stubs.c:418-434`. The closure's result is returned to FFmpeg as is.
`writeout` (`aviobuf.c` n8.1.3 `:136-166`, same at n7.1.5 and n9.0.2) tests
it for a negative value only and advances the position by the full length.
A closure that writes part of the range, as `Unix.write` may, loses the rest
silently.

**narrowed.** The original also claimed loss above 32768 bytes per call
(`len = MIN(BUFLEN, buf_size)`). That cap never binds in the three releases:
the context is created with a 32768-byte buffer, `direct` is 0 for a context
from `avio_alloc_context` and nothing sets it, and the only buffer
enlargement on a context (`ffio_realloc_buf`, from `seek.c`) is on the
demuxing side, so `flush_buffer` never passes more than 32768 bytes.

### G5. No protection against concurrent use of one container

`av/av_stubs.c:112-164` and every stub that releases the lock. While one
thread is inside `av_read_frame` or a write with the lock released, another
can call `close` and free the format context under it. Nothing documents or enforces
single-threaded use per container.

### G6. Callback closures are global roots: a closure that references its container pins it

`av/av_stubs.c:718,1650,552-565,963,1814`. The interrupt closure and (via
the I/O object) the read/write/seek closures are rooted until the container
is released. If one of them captures the container, the container is never
unreachable, so the `Gc.finalise` cleanup never runs. Only explicit `close`
breaks the cycle.

### G7. 64-bit overflow in time conversions

`av/av_stubs.c:1034`, `:1484-1492`. `duration * second_fractions * num` and
`timestamp * num` are computed before the division. With `` `Nanosecond``
and the container time base this overflows for about 9223 s and beyond
(2 h 33 min).

### G8. `seek` converts `ts` as a time whatever the flags

`av/av_stubs.c:1484-1495`. With `Seek_flag_byte` or `Seek_flag_frame` the
target is a byte offset or frame number, yet it is still rescaled by the
time base and time format.

### G9. Frame-mode decoders ignore the configure callback's options and `pkt_timebase`

`av/av_stubs.c:1128-1136`. The decoder is opened with a null dictionary; the
per-stream options only reach `avformat_find_stream_info`. `pkt_timebase`
is not set on the decoder context.

### G10. Hardware upload copies pixel data only

`av/av_stubs.c:2256-2291`. The frame sent to the encoder is the new
hardware frame; `av_frame_copy_props` is not called, so the caller's `pts`
and other properties are not carried over. See V3.

### G11. Subtitle encode buffer is fixed at 4096 bytes

`av/av_stubs.c:2384-2409`. Larger encoded subtitles (bitmap formats) fail in
the encoder. The written packet carries no key flag and ignores
`start_display_time` for its pts.

### G12. Unchecked allocations and indexes

`av/av_stubs.c:801-803` (`av_calloc` results used unchecked), `:1745`
(`av_strndup` unchecked), `:2004` (`initialize_stream_copy` index),
`:993`, `:1871` (metadata stream index), `:2612-2616` (H.264 extradata read
without consulting `extradata_size`).

### G13. Exported control-message helpers need the runtime lock

`av/av_stubs.c:174-176`, `:329-344`; callers at
`avdevice/avdevice_stubs.c:138-142`, `:240-243`. Both helpers call `Av_val`
(which can raise through an OCaml callback) and the setter registers a
root. `avdevice` calls them after `caml_release_runtime_system()`. Nothing
in the header states the requirement.

### G14. `write_packet` consumes the caller's packet

`av/av_stubs.c:2166`, `:2189-2194`. Timestamps, stream index and `pos` are
rewritten in the OCaml-owned packet, which is then handed to the muxer. The
`.mli` does not say the packet is altered. See V2.

**checked.** With `~interleaved:true` (the default) a 16-byte packet reads `size=0 pts=none` after `write_packet`; with `~interleaved:false` it reads `size=16 pts=0`. See V2.

## API gaps

### P1. `reopen_output_stream` leads to a state with no exit

`av/av.mli:276`, `av/av_stubs.c:1829-1844`. Undocumented. It replaces `pb`
with a dynamic memory buffer. No operation reads that buffer back, restores
the previous I/O context or closes the buffer; the previous context (file or
custom) is orphaned, and at close a file-based output calls `avio_closep` on
a dynamic buffer. Not used anywhere else in the tree.

**worse than reported; checked.** `open_output "p1.wav"`, `new_audio_stream`, `reopen_output_stream`, `close`: `free(): invalid pointer`, abort (exit 134), output file 0 bytes; without the reopen the same sequence closes cleanly. `avio_close` hands the buffer's `opaque` (a `DynBuffer`) to `ffurl_close` (`avio.c` n8.1.3 `:626-643`). Beyond the report: bytes still buffered in the replaced context are never flushed, and its file descriptor is never closed.

### P2. `get_frame_size` is typeable on streams that have no encoder

`av/av.mli:173`, `av/av_stubs.c:295-303`. `new_stream_copy` with audio
parameters returns ``(output, audio, [ `Packet ]) stream``, which
`get_frame_size` accepts; the stub then dereferences a null codec context.

**checked.** `new_stream_copy` with audio parameters, then `get_frame_size`: segfault (exit 139).

### P3. `write_frame` on a subtitle stream has no constructible argument

`av/av.mli:431-435`. For `'media = subtitle` it requires a
`subtitle Avutil.frame`, while subtitle frames are `Avutil.Subtitle.frame`.
`write_subtitle_frame` is the only usable route, and `?on_keyframe` has no
effect there.

**confirmed (second read).** Searched every `.mli` for a producer of `subtitle Avutil.frame`: frames come from `Avcodec.decode` (audio and video decoders only), `Avfilter` and the `Avutil.Audio`/`Video` constructors.

### P4. No way to remove or repair a stream after a failed creation

See G1: the `.mli` offers no operation to drop the stream, and `close` then
raises.

**checked.** See the run under G1: after the failed creation `close` raises on every call and no operation removes the stream.

### P5. `open_output_format` on a file-based format

`av/av.mli:256-261`, `av/av_stubs.c:1711-1714`. With a format lacking
`AVFMT_NOFILE` the routine calls `avio_open2` with a null file name. The
type admits any output format. See V5.

**worse than reported; checked.** `Av.open_output_format` on the `mp4` format: segfault (exit 139), not an error. `avio_open2` reaches `url_find_protocol`, which calls `strspn(filename, ...)` on the null pointer (`avio.c` `:311` at n7.1.5 and n8.1.3, `:316` at n9.0.2).

### P6. `output_started`, `set_output_metadata` and the header state

The header-written state can only be advanced by writing a packet or frame,
or by `close`; there is no operation to write the header explicitly, so
values the muxer fixes at header time (stream time bases) cannot be read
before the first write.

**confirmed (second read).** `avformat_write_header` is called at `:2179` and `:2210` only; `flush` returns early while the header is unwritten (`:2478`), and `close` reaches it only through an audio or video encoder flush.

## To verify

### V1. A custom write function's short count is treated as a full write

**settled: narrowed.** The short-count half holds: `writeout` treats only a
negative return as an error and advances by the requested length at all
three tags. The 32768-byte half does not arise: see G4.

### V2. `av_interleaved_write_frame` takes ownership of the packet reference

From memory: on return the packet is blank. `av_write_frame` leaves it
untouched. If so, after `write_packet` the caller's `Avcodec.Packet.t` is
empty in interleaved mode and rescaled in non-interleaved mode.

**settled: holds.** `avformat.h` documents that the function takes ownership and returns a blank packet; `mux.c` n8.1.3 `:856` moves the reference and `:1230` unrefs on error. `av_write_frame` works on an internal copy (`:1196-1213`). Run: see G14.

### V3. `av_hwframe_transfer_data` does not copy frame properties

From memory of `hwcontext.h`. If so G10 drops `pts` on every hardware
encode.

### V4. `avio_tell` does not call the seek function

From memory: `avio_tell` is `avio_seek(s, 0, SEEK_CUR)`, answered from the
buffer position without invoking the user seek function. The comment at
`av/av_stubs.c:2502-2503` says otherwise.

**settled: holds.** `aviobuf.c` n8.1.3 `:258-261`: for `SEEK_CUR` with offset 0 `avio_seek` returns `pos + (buf_ptr - buffer)` before any call to `s->seek`.

### V5. `avio_open2` with a null URL

Behaviour not known from memory (error or crash).

**settled: crash.** Null pointer passed to `strspn` in `url_find_protocol`. Run: see P5.

### V6. `avformat_close_input` leaves a caller-supplied `pb` alone

The release routine relies on libavformat setting `AVFMT_FLAG_CUSTOM_IO`
when `pb` is preset, so that closing a custom input does not free the I/O
object's `AVIOContext`. On the early failure path of `open_av`
(`av/av_stubs.c:704-714,741`, allocation failure before
`avformat_open_input`) that flag is not yet set and `avformat_close_input`
may close the custom context, which the I/O object frees again later.

### V7. `av_read_frame` keeps returning `AVERROR_EOF`

The drain step relies on each `read_input` after end of file getting
`AVERROR_EOF` again.

### V8. `av_opt_set_dict` on the format context does not reach muxer private options

The code applies the dictionary to `priv_data` separately
(`av/av_stubs.c:1683-1684`), which implies it.
