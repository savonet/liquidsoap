# avcodec — findings

Paths are relative to `avcodec/` unless prefixed. "Checked" items were run
against the installed FFmpeg (libavcodec 63.14.100, libavutil 61.7.100,
aarch64, gcc) with a small C program that includes the generated
`codec_id_stubs.h` from `_build` and links libavcodec; it replicates the
stub's statements and does not go through OCaml. Verification verdicts were
added by a second reader; its runs used OCaml programs linked against the
built `avcodec.cmxa` and the same FFmpeg, and FFmpeg source read at tags
n7.1.5, n8.1.3 and n9.0.2.

## Defects

### Codec enumeration mangles ids with a negative OCaml representation

- `avcodec_stubs.c:1421`, `:1436-1456`; consumer `avcodec.ml:57-63`.
- The matched table entry (column 0, an OCaml `value` held as `int64_t`) is
  stored in `enum AVCodecID id`, then written with `Store_field(_id, 0, id)`.
  `enum AVCodecID` has no negative enumerator, so gcc makes it `unsigned
int`: a negative `value` is truncated to 32 bits and zero-extended back.
- The result is an immediate that equals no polymorphic-variant hash.
  `codecs_of` then drops the codec (`List.mem id codec_ids` is false), so
  `Audio/Video/Subtitle.encoders` and `.decoders` silently miss those codecs.
- **Checked** for the C part: `sizeof(enum AVCodecID) == 4`, unsigned; of 721
  codecs, 714 get an id and 191 of those are mangled (e.g. `cinepak`: stored
  3984935419, table value −310031877).
- **Checked** from OCaml (verification): a program linked against the built
  `avcodec.cmxa` and libavcodec 63.14.100 listed the six lists by
  `get_name` and compared them with `ffmpeg -decoders` / `-encoders` of the
  same install. It printed `video decoders has cinepak: false`, `dnxhd:
false`, `rawvideo: false`, `h264: true`; the raw id returned for `cinepak`
  is the OCaml int 1992467709 against −155015939 for `` `Cinepak ``; 191 of
  714 ids equal no member of any `codec_ids`. Missing / total per list:
  video decoders 47/280, video encoders 21/106, audio decoders 93/223,
  audio encoders 25/79, subtitle decoders 9/22, subtitle encoders 3/11; no
  list has an entry FFmpeg lacks. Missing encoders include `prores`,
  `dnxhd`, `rawvideo`, `pcm_alaw`, `pcm_mulaw`, `pcm_u8`, `g722`.
  `Video.find_decoder_by_name "cinepak"` followed by `get_id` still gives
  `` `Cinepak ``: only the enumeration is affected.
- 198 entries are missing in total: the 191 mangled ones and the 7 codecs
  with no id (see "Codecs typed audio/video with an id of the unknown
  family" under Gaps).

### Bitstream filter options are never applied

- `avcodec_stubs.c:1580`.
- `av_opt_set_dict(bsf, &options)` searches the `AVBSFContext` only. Its
  class has no options of its own; a filter's options live in `priv_data`
  and need `AV_OPT_SEARCH_CHILDREN`.
- Every option passed to `BitstreamFilter.init ?opts` is reported back as
  unused and has no effect, while `filter.options` advertises them.
- **Checked**: on the `noise` filter, `av_opt_set_dict` with `amount=5`
  leaves 1 entry; `av_opt_set_dict2(…, AV_OPT_SEARCH_CHILDREN)` leaves 0.
- **Confirmed (second read)** at n7.1.5, n8.1.3 and n9.0.2:
  `libavcodec/bsf.c` `bsf_class` has `child_next` and no `.option`, and
  `av_opt_set_dict` is `av_opt_set_dict2(obj, options, 0)` in
  `libavutil/opt.c`; nothing after it in the stub (`av_bsf_init`) reads the
  dictionary, which is what could have refuted it.

### `Packet.add_side_data` ignores failure and leaks the buffer

- `avcodec_stubs.c:194`, `:206`.
- The return of `av_packet_add_side_data` is dropped. On failure FFmpeg does
  not take the `av_malloc`'d buffer, so it leaks and the caller sees success.
- **Narrowed.** `libavcodec/packet.c` at n7.1.5 and n9.0.2 returns without
  touching `data` on both failure paths, so the claim holds, but the paths
  are `av_realloc` failure and more than `AV_PKT_DATA_NB` entries only. An
  existing entry of the same type is replaced (its buffer freed), so
  repeated calls from this binding cannot reach the count limit: the leak
  is out-of-memory only.

### Metadata side data is written without the trailing NUL

- `avcodec.ml:152-153` (write), `:167-172` (read).
- `concat_meta` joins `key NUL value` pairs with single NULs and no final
  NUL. FFmpeg's packed-dictionary format terminates every string, the last
  included, and `av_packet_unpack_dictionary` rejects data whose last byte
  is not NUL.
- The reader accepts both forms, so the binding round-trips with itself and
  reads FFmpeg's output, but what it writes is not what FFmpeg consumers
  expect.
- The reader also returns the pairs of one entry in reverse order
  (`split_meta` conses), so `side_data` after `add_side_data` gives the list
  reversed.
- **Confirmed (second read).** `av_packet_unpack_dictionary` at n7.1.5 and
  n9.0.2 returns `AVERROR_INVALIDDATA` when `end[-1]` is non-zero, and
  `av_packet_pack_dictionary` copies `strlen + 1` bytes for every key and
  value. The last-byte test runs before any `strlen`, so an FFmpeg consumer
  rejects the payload and does not read past it. A last pair with an empty
  value does end in NUL and is rejected by the `val >= end` test instead.

### `Packet.dup` ignores the result of `av_packet_ref`

- `avcodec_stubs.c:291`.
- On failure the new packet is blank and is returned as a valid duplicate.
- **Confirmed (second read).** `av_packet_ref` at n7.1.5 and n9.0.2 ends its
  failure path with `av_packet_unref(dst)` and returns the error; its
  failures are allocation failures in `av_packet_copy_props`,
  `packet_alloc` or `av_buffer_ref`.

### `Avcodec.params` leaks its temporary on the copy's error path

- `avcodec_stubs.c:508-510`.
- `value_of_codec_parameters_copy` can raise (allocation, copy error, custom
  block allocation); the temporary `params` is freed only after it returns.
- **Confirmed (second read).** `value_of_codec_parameters_copy`
  (`avcodec_stubs.c:84-103`) raises from three places and takes no cleanup
  argument; `params` is not held by any custom block at that point. All
  three are allocation failures.

### `.mli` comments that do not match the code

- `avcodec.mli:284-300`, `:407-423`: the `Video` and `Subtitle` `find_*`
  comments say "is not an audio codec"; the stubs check video / subtitle.
- `avcodec.mli:142-143`, `:266-267`, `:389-390`: the "aac and libfdk_aac"
  caveat is repeated for video and subtitle ids.
- `avcodec.mli:87`: `dup` "referring the same data" holds only for
  reference-counted sources; FFmpeg copies otherwise.
- `avcodec.mli:500`: `flush_encoder encoder` omits `f`.
- **Confirmed (second read).** Each cited comment was read against its stub;
  `dup` copies when `src->buf` is null (`av_packet_ref`); packets from
  `Packet.create` are reference-counted.

## Asymmetries

### End of stream: decoder receive raises, encoder receive returns nothing

- `avcodec_stubs.c:697-705` vs `:810-817`; `avcodec.ml:559-563` vs
  `:584-588`.
- The decoder receive stub raises ``Error `Eof``; `flush_decoder` catches it
  around the whole body, so an ``Error `Eof`` raised by the user function is
  swallowed too. The encoder receive stub maps EOF to "nothing".
- **Confirmed (second read).** No other handler sits between the receive
  stub and `flush_decoder`'s `try`, and `decode` has none, so a `decode`
  after `flush_decoder` raises ``Error `Eof`` from the send.

### Second flush: decoder silent, encoder raises

- `avcodec_stubs.c:741-744`, `avcodec.ml:559-563`, `:584-588`.
- The encoder keeps its own `flushed` flag and raises ``Error `Eof`` on any
  send after a flush, so `flush_encoder` twice raises. The decoder has no
  flag; `flush_decoder` twice returns unit. `flushed` is set before the
  send, so a failed end-of-stream send still leaves the encoder flushed.
- **Narrowed.** The second `flush_decoder` is silent only because of the
  `try`: `avcodec_send_packet` returns `AVERROR_EOF` once
  `draining_started` is set (`libavcodec/decode.c`, n8.1.3), the stub
  raises it, and `flush_decoder` swallows it. Both codecs refuse the second
  end-of-stream send; the asymmetry is in the OCaml wrappers, one of which
  catches `Eof`.

### Bitstream filter receive has no "nothing yet" result

- `avcodec_stubs.c:1662-1665`, `avcodec.ml:632`.
- `BitstreamFilter.receive_packet` raises on `EAGAIN` and `EOF`; the encoder
  twin returns an option and has a drain loop. No loop is provided for
  filters.
- **Confirmed (second read).** The stub raises on every negative return and
  `avcodec.ml` exposes the external unwrapped.

### Hardware transfers run with the runtime lock held

- `avcodec_stubs.c:714`, `:753`, `:764`.
- `av_hwframe_get_buffer` and `av_hwframe_transfer_data` are called between
  the lock-releasing send/receive calls without releasing the lock.
- **Confirmed (second read).** The lock is reacquired at `:695` before the
  download and released at `:774` after the upload; no release surrounds
  the three calls.

### `hw_configs` is returned reversed, the supported lists are not

- `avcodec_stubs.c:1080-1106`, `avcodec.ml:108-109` vs `:263-264` etc.
- The supported-list externals are wrapped with `List.rev`; `hw_configs` is
  exposed raw, so configs come last-first and `methods` in reverse table
  order. `all_codecs` and `BitstreamFilter.filters` are also in reverse
  iteration order.
- **Confirmed (second read).** `ocaml_avcodec_hw_methods` prepends each
  config and each method to its list, and `avcodec.ml:108` binds the
  external with no wrapper.

### Missing bitstream filter raises `Not_found`

- `avcodec_stubs.c:1560-1562`.
- Every other lookup failure raises `Avutil.Error` (`Encoder_not_found`,
  `Decoder_not_found`); the shared error path has a `Bsf_not_found` variant
  that this stub does not use.
- **Confirmed (second read).** `caml_raise_not_found()` at `:1561`;
  `` `Bsf_not_found `` exists at `avutil/avutil.ml:81` and
  `avutil/avutil_stubs.c:44`.

## Gaps

### Flush passes the C integer 0 as an OCaml value and roots it

- `avcodec_stubs.c:733` with `:664-666`; `:827` with `:786-789`.
- `ocaml_avcodec_send_packet(_ctx, 0)` / `ocaml_avcodec_send_frame(_ctx, 0)`
  register a null word with `CAMLparam2` and keep it rooted while the runtime
  lock is released around the FFmpeg call. Null is not an OCaml value; the
  code relies on the GC tolerating it in a local root. The project builds
  with OCaml 5.5.0.
- **Narrowed: not a defect on OCaml 5.5.0.** What is rooted is the callee's
  own parameter slots: `CAMLparam2(_ctx, _packet)` registers the addresses
  of `_ctx` and `_packet`, and `_packet` holds the word 0 for the whole
  call, lock release included. `caml_do_local_roots` (`runtime/roots.c:59`
  at tag 5.5.0) skips a root whose content is 0 (`if (*sp != 0)`), and the
  systhreads scan of a blocked thread goes through the same function
  (`otherlibs/systhreads/st_stubs.c:225`), so the scanning action never
  sees the null word. Nothing writes through the slot. What remains is a
  reliance on that runtime check, which the C interface does not document;
  a `Val_none`-style sentinel or a separate helper taking `AVPacket *`
  would not need it.

### Hardware frames lose their properties

- `avcodec_stubs.c:753-771` (upload), `:707-723` (download).
- Neither direction calls `av_frame_copy_props`. On upload the frame sent to
  the encoder carries no pts; on download the frame returned to OCaml
  carries none of the decoder's timestamps or metadata. Nothing states this.
- **Confirmed (second read).** `av_hwframe_transfer_data`
  (`libavutil/hwcontext.c`, n7.1.5 and n9.0.2) has no
  `av_frame_copy_props`; in `hwcontext_vaapi.c` only the map path has one;
  `fftools/ffmpeg_dec.c:370` (n8.1.3) copies the properties itself after
  the transfer.

### The download branch of the decoder cannot be configured

- `avcodec_stubs.c:707`.
- The receive stub handles a codec context with `hw_frames_ctx`, but decoder
  creation sets neither `hw_device_ctx` nor `hw_frames_ctx` nor
  `get_format`. The branch runs only if a decoder sets the field by itself.
- **Confirmed (second read).** `create_AVCodecContext`
  (`avcodec_stubs.c:42-68`) is the only decoder path and sets parameters
  and `thread_count` only.

### `BitstreamFilter.send_packet` empties the caller's packet

- `avcodec_stubs.c:1620`.
- `av_bsf_send_packet` moves the content out of the packet. The OCaml value
  stays usable but is empty; sending it again signals end of stream. The
  `.mli` says nothing, and decoder sends leave packets intact.
- **Checked**: after a successful send to the `null` filter, `size == 0` and
  `data == NULL`.

### A raising user function leaves output buffered

- `avcodec.ml:548-557`, `:573-582`.
- If `f` raises mid-drain, frames or packets remain inside FFmpeg. The next
  `decode`/`encode` sends first; whether that send returns `EAGAIN` depends
  on the FFmpeg version, and nothing drains without sending.
- **Read only.**

### Derived encoder options that FFmpeg does not consume vanish

- `avcodec.ml:331-344`, `:462-481`; derivation in `avutil/avutil.ml:822-876`.
- Derived keys live in a copy of the caller's table, and `filter_opts`
  reports only against the caller's table. An unconsumed derived key is
  dropped without notice.
- **Checked** on libavcodec 63.14.100: `av_opt_find` on a bare
  `AVCodecContext` finds `pixel_format` but neither `r` nor
  `channel_layout`. So `?frame_rate` of `Video.create_encoder` has no effect
  unless the codec has a private `r` option, and the audio layout reaches
  the context only through the direct `av_channel_layout_copy`.
- **Worse than reported**, the same at n7.1.5, n8.1.3 and n9.0.2:
  `libavcodec/options_table.h` has `ar`, `time_base`, `pixel_format`,
  `video_size`, `colorspace`, `color_range` and `ch_layout`, and none of
  `r`, `channel_layout`, `sample_fmt`, `ac` or `framerate`. No codec in
  `libavcodec/` declares a private `r`, `channel_layout` or `sample_fmt`
  option either (the only `"r"` is the `pcm_rechunk` bitstream filter). So
  `?frame_rate` has no effect with any in-tree codec:
  `AVCodecContext.framerate` stays unset and nothing reports it. Three of
  the eight derived keys are never consumed: `r`, `channel_layout` and
  `sample_fmt`; the last was not listed by the first reader and is
  harmless only because the stub also assigns `ctx->sample_fmt` directly.
- `avcodec_open2` applies the dictionary with one
  `av_opt_set_dict2(avctx, options, AV_OPT_SEARCH_CHILDREN)`
  (`libavcodec/avcodec.c`), and `av_opt_find2` searches children first: a
  codec-private option shadows a generic one of the same name.

### Custom-block allocation failures leak the C object

- `avcodec_stubs.c:100`, `:130`, `:476`, `:544`, `:611`, `:1599`.
- The C object exists before `caml_alloc_custom*`; if that raises, nothing
  frees it. Relies on the allocation not failing.
- **Confirmed (second read).** All six sites read; none frees on that path.
  The encoder constructors (`:544`, `:611`) also leak the option dictionary
  there. `caml_alloc_custom` fails with `Out_of_memory` only.

### The pre-58.9.100 guard is unreachable

- `avcodec_stubs.c:29-31`.
- The file uses `AVChannelLayout` and `av_codec_iterate` unconditionally,
  which need a much newer libavcodec. No minimum version is stated anywhere
  in these files.
- **Confirmed (second read).** `ocaml_avcodec_get_next_codec` calls
  `av_codec_iterate` with no version guard.

### No object is protected against concurrent use

- `avcodec_stubs.c:669-671`, `:693-695`, `:774-776`, `:806-808`,
  `:1619-1621`, `:1658-1660`.
- The lock is released around calls on the codec/filter context and nothing
  prevents another OCaml thread from entering the same context or mutating
  the packet being sent.
- **Read only.**

### Codecs typed audio/video with an id of the unknown family are never listed

- `avcodec_stubs.c:1436-1449`; `avcodec.ml:57-63`.
- **Found in verification, checked.** The enumeration returns no id for 7
  entries of libavcodec 63.14.100: the decoders `bintext`, `xbin`, `idf`,
  `vnull`, `anull` and the encoders `vnull`, `anull`. FFmpeg types
  them video or audio (`ffmpeg -decoders` shows them with `V` / `A`), but
  their ids sit after `AV_CODEC_ID_FIRST_UNKNOWN` in the header, so the
  generated audio, video and subtitle tables do not hold them. They are
  absent from every `encoders` / `decoders` list, independently of the
  mangling defect. `find_*_by_name` accepts them (it tests `codec->type`);
  `get_id` on the result then raises `Failure` from the table lookup (read,
  not run).

## API gaps

**Confirmed (second read)**, all eleven: each was checked against
`avcodec.mli` and the stub it names (`set_flags` and `avcodec_flush_buffers`
appear nowhere; `ocaml_avcodec_name` and `ocaml_avcodec_get_name` both copy
`codec->name`; the duration and position getters test `== 0` and `== -1`).

- **No `Packet.set_flags`.** Every other mutable packet field has a setter;
  a packet made with `Packet.create` cannot be marked as a keyframe.
  `avcodec.mli:90-91`.
- **No way out of the flushed/drained state.** `flush_decoder` and
  `flush_encoder` are terminal and there is no reset
  (`avcodec_flush_buffers`), and no explicit close; a decoder cannot be
  reused across a seek. `avcodec.mli:486-504`.
- **No release operation** for decoders, encoders, filters, packets or
  params: all are freed by the GC only, and only packets report their size
  to it. `avcodec.mli:8-11`, `:63`, `:469`.
- **Decoders take neither options nor a hardware context**, while encoders
  take both and the decoder receive path handles hardware frames.
  `avcodec.mli:207`, `:328`.
- **`subtitle` and `` `Data `` decoders/encoders cannot be built here.** The
  types `subtitle decoder`, `subtitle encoder` exist and `decode`/`encode`
  accept them, but no constructor is exported, and the frame-based
  send/receive API does not serve subtitles. `avcodec.mli:385-439`,
  `:486-504`.
- **`capabilities` is typed for encoders only** although the stub reads any
  codec; decoders' capabilities are unreachable. `avcodec.mli:45`.
- **`Video.get_sample_aspect_ratio` and `Video.get_pixel_aspect` read the
  same field** with different shapes (raw rational vs `None` when the
  numerator is 0). `avcodec.mli:372`, `:378`.
- **`Packet.duration` cannot represent 0** distinctly from "unset", and
  `position` cannot represent −1. `avcodec.mli:116-125`.
- **`BitstreamFilter.receive_packet` forces exception handling for normal
  flow** (`Eagain`, `Eof`), unlike the codec API in the same module.
  `avcodec.mli:479`.
- **`Unknown` has no `descriptor`**, while `Audio`, `Video` and `Subtitle`
  have one. `avcodec.mli:441-455`.
- **Duplicates with identical semantics**: `Avcodec.name` and
  `Audio/Video/Subtitle.get_name`; `Packet.content` and `Packet.to_bytes`
  (differing only in result type). `avcodec.mli:39`, `:128-134`, `:231`.

## To verify

- `av_packet_unpack_dictionary` rejects a payload whose last byte is not
  NUL, and `av_packet_pack_dictionary` terminates every string. **Settled:
  true** at n7.1.5 and n9.0.2.
- `av_packet_add_side_data` leaves ownership of `data` with the caller on
  failure. **Settled: true** at n7.1.5 and n9.0.2.
- `av_packet_ref` unrefs the destination on failure, leaving a blank packet.
  **Settled: true** at n7.1.5 and n9.0.2.
- `av_hwframe_transfer_data` copies no frame properties in either direction.
  **Settled: true** for the generic layer and VAAPI at n7.1.5 to n9.0.2;
  other backends not read.
- `av_new_packet` returns exactly 0 on success; the stub tests `!= 0`
  (`avcodec_stubs.c:146`).
- `avcodec_get_supported_config` accepts a null `out_num_configs`.
  **Checked** on 63.14.100 for the `aac` encoder sample rates: returns 0
  with a non-null array.
- Colour-space and colour-range arrays are terminated by
  `AVCOL_SPC_UNSPECIFIED` / `AVCOL_RANGE_UNSPECIFIED`
  (`avcodec_stubs.c:1305-1309`, `:1333-1336`; stated by the code comment).
- `avcodec_open2` applies the option dictionary after the direct
  `thread_count = 0` assignment, so a `threads` option overrides it
  (`avcodec_stubs.h:13-16`, stated by the comment).
- When the same key is present twice in the flattened option array, the
  derived value is set last and wins (`Hashtbl.fold` order in
  `avutil/avutil.ml:808-812`).
- Whether `avcodec_send_packet` returns `EAGAIN` while a frame is still
  buffered differs across FFmpeg versions.
- `AVCodecHWConfig.device_type` can be `AV_HWDEVICE_TYPE_NONE` for
  internal/ad-hoc methods; whether the `avutil` device-type conversion maps
  it decides if `hw_configs` can raise (`avcodec_stubs.c:1100`).
- `AVCodecParameters.format` can be `AV_SAMPLE_FMT_NONE`; whether the
  `avutil` sample-format conversion maps it decides if
  `Audio.get_sample_format` can raise (`avcodec_stubs.c:1215-1219`).
- Whether the OCaml 5 major GC tolerates a null word in a local root (see
  the flush gap). **Settled: yes** on 5.5.0, `runtime/roots.c:59` skips
  it.

## Refuted

### Hardware upload leaks the pool frame on one path

- `avcodec_stubs.c:760-762`.
- When `hw_frame->hw_frames_ctx` is null the stub raises `Out_of_memory`
  without `av_frame_free(&hw_frame)`; the two neighbouring error paths free
  it.
- **Refuted.** The branch cannot run: `av_hwframe_get_buffer`
  (`libavutil/hwcontext.c`, n7.1.5 and n9.0.2) sets `frame->hw_frames_ctx`
  from `av_buffer_ref` and returns `AVERROR(ENOMEM)` when that fails, on
  both its derived and pooled paths, so after a non-negative return the
  field is never null. The missing free is dead code, not a leak.

### `filter.options` may wrap a null class

- `avcodec_stubs.c:1523`.
- `filter->priv_class` is null for filters without options; it is wrapped
  unchecked into an `Avutil.Options.t`. Safety depends on the `avutil`
  option-listing code.
- **Refuted** as a risk. The only consumer, the option iterator at
  `avutil/avutil_stubs.c:1798-1806`, reads the class and returns "no
  option" when it is null, on the first call and on every continuation.
