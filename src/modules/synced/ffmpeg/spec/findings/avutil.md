# avutil — findings

"Checked" items were run against the library built from this tree
(`dune build ./avutil/avutil.cmxa ./avutil/libavutil_stubs.a`, linked into a
throwaway program in the session scratchpad) with libavutil 61.7.100.

Verification pass (second reader): verdicts are in bold after each location.
Runs used the library built from this tree into
`~/.cache/ffmpeg-spec-builds/verify-avutil` (OCaml 5.5.0, the FFmpeg under
`~/.local/ffmpeg`), linked into throwaway programs. FFmpeg behaviour was
read at tags `n7.1.5` and `n9.0.2`.

## Defects

### Log messages of one batch are delivered newest first

`avutil/avutil_stubs.c:353-376`. **Checked.** Second read: the check is
sound; the second loop prepends while walking oldest to newest.
The pending list is a LIFO stack. The stub reverses it to chronological
order, then builds the OCaml list by prepending while walking that
chronological chain, which reverses it again. Three failing
`Video.create_frame` calls logging sizes 100001, 100002, 100003 before the
log thread ran were delivered as 100003, 100002, 100001.

### `set_callback` right after `clear_callback` is lost

`avutil/avutil.ml:204-223`. **Checked.** Second read: the check is sound.
`clear_callback` sets the stop flag and leaves `log_thread` true until the
log thread wakes. A `set_callback` in that interval sees `log_thread = true`
and neither reinstalls the C callback nor clears the stop flag. The thread
then stops. The new callback is never called and FFmpeg's default callback
stays active. Run: `clear_callback (); set_callback f;` then a logging call
printed to stderr and never reached `f`; the same after a 0.3 s pause
worked.

### An exception in the log callback disables logging for good

`avutil/avutil.ml:185-213`. **Narrowed** (by a run).
The callback runs on the log thread inside `mutexify`. An exception ends
the thread with `log_thread` still true and the C callback still installed.
The messages of the same batch that follow the failing one are lost. Later
messages are queued and stay queued until `clear_callback` is called:
`clear_callback` passes them to the current callback on the calling thread
and restores FFmpeg's default callback (`avutil.ml:219-220`). No later
`set_callback` restarts a thread, because `log_thread` is never reset, so
after that `clear_callback` every message goes to FFmpeg's default
callback. Run: a callback raising `Failure "boom"` on the first message
(stderr: `Thread 1 killed on uncaught exception Failure("boom")`), then
`set_callback count` and two logging calls printed `after reinstall: 0
delivered`; `clear_callback ()` printed `after clear_callback: 2
delivered`; a further `set_callback count` and one logging call printed
`after set_callback again: 2 delivered` and the message on stderr.

### `~search_children:false` behaves as `true`

`avutil/avutil.ml:770`, `avutil/avutil_stubs.c:1583`. **Confirmed (second
read).** Looked for an unwrapping step: the wrapper at `avutil.ml:773-775`
forwards `?search_children` as the option, the external declares the label
optional, the stub has no `Is_some`/`Field` on it, and the installed
`caml/mlvalues.h` defines `Bool_val` as `Int_val` (with a `bool` cast under
C23), non-zero for any pointer.
The external receives `?search_children` as a `bool option`. The stub
applies `Bool_val` to the option: `None` reads false, and both `Some false`
and `Some true` are heap pointers, which read true.

### Missing roots in subtitle stubs

**Checked**, both items.

- `avutil/avutil_stubs.c:1469-1483`, `1503`: `rects_list` (a plain C
  `value`) is read from `_content` before `value_of_subtitle` allocates the
  custom block, then walked afterwards. A minor collection in between moves
  the list and leaves `rects_list` stale. A freshly built `content` is
  typically in the minor heap.
- `avutil/avutil_stubs.c:1441-1442`: `value rect = value_of_rectangle(...)`
  is unrooted, then `List_add` allocates the cons cell before storing
  `rect`.

Runs (8 text rectangles per subtitle, 200000 iterations, random small
allocations between calls):

- `create_frame` alone on fresh contents, `get_content` never called:
  segmentation fault (exit 139) with the default minor heap and with
  `OCAMLRUNPARAM=s=4k`.
- `get_content` alone on a frame and content promoted beforehand, the
  results kept in a 512-slot array and compared later: `Fatal error:
allocation failure during minor GC` (exit 134) with `s=4k`; 0 mismatches
  with the default minor heap.

The `get_content` damage is delayed. The unrooted record is left in place
by the collection and the stale pointer keeps reading correctly until the
minor heap fills again, so a result that is compared and dropped at once
looks sound (200000 such calls, 0 mismatches, with either heap size). A
result kept across a later collection is what breaks.

### OCaml string used while the runtime lock is released

`avutil/avutil_stubs.c:2088-2099`. **Confirmed (second read).** Looked for
a copy of the string or a call made with the lock held: there is neither;
`CAMLparam3` roots `_name` and does not pin it.
`name = String_val(_name)` is passed to `av_hwdevice_ctx_create` between
`caml_release_runtime_system` and `caml_acquire_runtime_system`. Another
thread's collection may move the string during the call. The path needs a
non-empty `~device`; the default `""` passes `NULL`.
`HwDeviceType_val(_device_type)` is also evaluated after the release
(`2097`). It reads no heap memory, and its failure branch, unreachable for
a well-typed variant, would call back into OCaml without the lock.

### Option constants are never reported; `values` is always empty

`avutil/avutil_stubs.c:1852-1854`, `avutil/avutil.ml:604-688`, `752-759`.
**Confirmed (second read).** Looked for another path that returns a
`` `Constant ``: `type_of_av_opt_type` maps the type to `PVV_Constant`
(`1762-1763`) and the `switch` raises before `_opt` is returned, with no
version guard; the driver catches the exception and resumes (`759`). One
more hazard in the dead path: `default_string` (`2017-2020`) would read a
constant's integer or double default as a `char *`.
The iterator raises "not implemented" for `AV_OPT_TYPE_CONST`, so the OCaml
driver's `` `Constant `` branch never runs, the constants table stays empty
and `constant_of_opt` is dead. `entry.values` ("Pre-defined options" in
`avutil.mli:456-457`) is always `[]`. The dead path has further
inconsistencies a rewrite should not copy as-is: a `` `Flags `` option with
constants is rebuilt as `` `Int64 ``; a `` `Bool `` constant maps 0 to
`true`; `` `Array `` and `` `Binary `` options with a unit raise
`Failure "Incompatible constant!"`.

### `Child_consts` is tested against a macro that does not exist

`avutil/avutil_stubs.c:2055`. **Checked** (grep of the installed
`libavutil/opt.h`: `AV_OPT_FLAG_CHILD_CONSTS` is defined,
`AV_OPT_FLAG_AV_OPT_FLAG_CHILD_CONSTS` is not). Second read: the check is
sound.
The flag always maps to 0 and is never reported.

### Chroma planes are exposed with the luma height

`avutil/avutil_stubs.c:1295`. **Checked.** **Narrowed** in verification on
the palette sentence only.
Every plane's bigarray has length `linesize[i] * frame->height`. For a
64x48 `yuv420p` frame, planes 1 and 2 reported 1536 bytes (48 rows of 32)
where the plane holds 24 rows. The bigarray extends past the plane, and for
the last plane past its buffer (see "Padding around the last plane"). A
negative linesize gives a negative dimension.
A palette plane is not exposed at all: `av_pix_fmt_count_planes` counts the
planes named by components, and `pal8` has one component in plane 0. Run:
``Video.create_frame 16 16 `Pal8`` then `frame_visit` printed `pal8: 1
plane(s) exposed, Pixel_format.planes=1`. The palette of a `pal8` frame
cannot be read or written through this module.

### `.mli` promises `Not_found` where the code raises something else

**Checked** for the first three.

- `avutil/avutil.mli:169`, `avutil_stubs.c:549-552`: `Channel_layout.find`
  raises ``Error (`Other AVERROR(EINVAL))`` ("Invalid argument"), not
  `Not_found`.
- `avutil/avutil.mli:189-191`, `avutil_stubs.c:532-542`:
  `Channel_layout.get_default` never raises; `get_default 99` returned an
  unspecified-order layout described as "99 channels".
- `avutil/avutil.mli:217`, `avutil_stubs.c:631-637`:
  `Sample_format.find_id` raises `Error (`Failure "Could not find OCaml
  value ...")`.
- `avutil/avutil.mli:307`, `avutil_stubs.c:820-826`:
  `Pixel_format.find_id`, same. **Confirmed (second read):** the stub
  returns `Val_PixelFormat(Int_val(_id))`, and every generated `Val_*`
  ends in the same `Fail("Could not find OCaml value ...")`
  (`gen_code/gen_code.ml:193`, generated `*_stubs.h`); no `Not_found`.

### `frame_set_metadata` leaks the new dictionary on error

`avutil/avutil_stubs.c:1002-1007`. **Confirmed (second read).** Checked
whether `av_dict_set` frees the dictionary itself on failure: at `n7.1.5`
and `n9.0.2` its error exit frees `*pm` only when the count is 0
(`libavutil/dict.c`, label `end:`), so a dictionary with at least one entry
leaks.
A failing `av_dict_set` raises without freeing the partially built
dictionary. The exported `ocaml_avutil_dict_of_options` frees in the same
situation (`avutil_stubs.c:113-116`).

### Subtitle finaliser dereferences `NULL` after a failed allocation

`avutil/avutil_stubs.c:1494-1512`. **Confirmed (second read).** Checked
`avsubtitle_free` at `n7.1.5` and `n9.0.2`: it reads `sub->rects[i]` and
`rect->data[0]` for every `i < num_rects` with no `NULL` test. The custom
block exists from line `1483`, so the finaliser runs. Reachable only on a
failed `av_calloc`, `av_mallocz`, `av_malloc` or `av_strdup`.
`num_rects` is set before `rects` is allocated, and `rects[i]` entries are
filled one by one. An `Out_of_memory` raised in between leaves `num_rects`
larger than the number of valid rectangles; `avsubtitle_free` in the
finaliser then reads through `NULL`.

### Option getter passes an uninitialised `AVChannelLayout` to FFmpeg

`avutil/avutil_stubs.c:1576`, `1680-1681`. **Found in verification** (read
only).
`av_opt_get_chlayout` ends in `av_channel_layout_copy(cl, dst)`, which
starts with `av_channel_layout_uninit(cl)` (`libavutil/opt.c`,
`libavutil/channel_layout.c`, `n7.1.5` and `n9.0.2`). `cl` is the stub's
uninitialised stack variable: when its `order` bytes happen to equal
`AV_CHANNEL_ORDER_CUSTOM`, `av_freep` is applied to the garbage `u.map`.
The two other temporaries (`547`, `1788`) go to
`av_channel_layout_from_string`, which assigns or clears the structure
before reading it, and `535` goes to `av_channel_layout_default`, which
only writes.

## Asymmetries

### `Frame.copy` holds the runtime lock, `frame_copy_samples` releases it

`avutil/avutil_stubs.c:1037` versus `1176-1179`. **Confirmed (second
read).** Looked for a release around the three other calls; none has one.
Both copy frame data. `av_frame_copy` on a video frame is the larger copy.
`av_frame_make_writable` (`1281`) and `av_frame_get_buffer` (`1057`,
`1098`) also run with the lock held.

### Temporary channel layout uninitialised in two places, not in the third

`avutil/avutil_stubs.c:555`, `1867` versus `1680-1687`. **Confirmed
(second read).** `av_opt_get_chlayout` returns an owned copy
(`av_channel_layout_copy`), and `value_of_channel_layout` copies it again.
The same variable is the subject of the defect "Option getter passes an
uninitialised `AVChannelLayout`".
`find` and the option-default path call `av_channel_layout_uninit` on the
temporary after copying it. The `Channel_layout` option getter does not, so
a custom-order layout returned by `av_opt_get_chlayout` leaks its map.
`get_default` (`537-539`) does not either; default layouts hold no
allocation.

### Colour `name` stubs do not guard a `NULL` name; format name stubs do

`avutil/avutil_stubs.c:639-643`, `658-662`, `677-681`, `696-700`, `715-719`
versus `607-624`, `828-844`. **Confirmed (second read).** The five FFmpeg
lookups return `NULL` for a value at or above the library's `*_NB`
(`libavutil/pixdesc.c`, `n7.1.5` and `n9.0.2`), which a variant generated
from newer headers than the loaded library reaches. These five stubs also
open with `CAMLparam0()` and leave their argument unregistered; it is an
immediate.
`caml_copy_string` is applied to the result of `av_color_space_name` and
its siblings without a `NULL` check. Sample and pixel format name lookups
return `None` on `NULL`.

### Log primitives disagree on who clears the C callback

`avutil/avutil.ml:192`, `219`. **Confirmed (second read).**
`clear_callback` restores the default callback itself, and the log thread
restores it again when it sees the stop flag.

## Gaps

### `frame_copy_samples` does not compare sample formats or reject negatives

`avutil/avutil_stubs.c:1159-1178`. **Read only.**
Planarity, plane count and bytes per sample all come from `dst->format`;
`src->format` is not read. A packed destination with a planar or narrower
source makes `av_samples_copy` read past the source plane. Negative
`src_offset`, `dst_offset` or `len` pass the bounds test.

### `Subtitle.create_frame` trusts the shape of `planes`

`avutil/avutil_stubs.c:1523-1538`. **Read only.**
The stub reads four elements of each array with no length check, and does
not relate a plane's byte size to `h * linesize`. The type
`data array * int array` admits any lengths.

### Bigarrays from `frame_visit` do not keep the frame alive

`avutil/avutil_stubs.c:1298-1300`, `avutil/avutil.ml:384-386`. **Read
only.**
The `.mli` states the rule (`avutil.mli:357-363`); nothing enforces it. A
plane kept after the visitor returns dangles once the frame is collected or
made writable again.

### The failure message buffer is process-wide

`avutil/avutil_stubs.h:33-38`, `avutil_stubs.c:38`. **Read only.**
`Fail` formats into one static buffer and then copies it. This relies on a
single runtime lock; with several domains two failures can interleave.

### The log callback is not re-entrant

`avutil/avutil.ml:185-223`. **Read only.**
The callback runs with `log_m` held. Calling `set_callback` or
`clear_callback` from inside it locks the same mutex again.

### Truncation of log lines and partial lines

`avutil/avutil_stubs.c:279-306`. **Read only.**
Each `av_log` call becomes one message of at most 1023 bytes; a line that
FFmpeg emits in several `av_log` calls arrives as several messages. A
message is dropped silently when its node cannot be allocated.

### `planes` returns a negative error code as a plane count

`avutil/avutil_stubs.c:808-813`. **Read only.**
`av_pix_fmt_count_planes` returns a negative `AVERROR` for a format with no
descriptor; the stub returns it as an `int`. `frame_visit` checks the same
call (`1287-1290`).

### Header declares a conversion that is defined nowhere

`avutil/avutil_stubs.h:88`, `92`. **Checked** (grep over the tree).
`AVSampleFormat_of_Sample_format` has a prototype and no definition;
`Sample_format_val` is an unused macro.

### `av_assert2` names fields removed from `AVFrame`

`avutil/avutil_stubs.c:1168-1170`. **Read only.**
`src->channel_layout`, `src->channels` and
`av_get_channel_layout_nb_channels` compile only because the macro discards
its argument at the default assert level.

### `avutil` links against libavcodec

`avutil/avutil_stubs.h:8`, `avutil_stubs.c:1312`. **Checked** (generated
`avutil_c_library_flags.sexp` is `-lavcodec -lavutil`).
`AVSubtitle`, `avsubtitle_free` and `SUBTITLE_BITMAP` are libavcodec. The
base library is not libavutil-only.

### `mk_audio_opts` can raise `Invalid_argument`

`avutil/avutil.ml:828-835`. **Confirmed (second read).**
`av_channel_layout_default` sets `AV_CHANNEL_ORDER_UNSPEC` when no default
has that channel count, and `get_mask` returns `None` for any order other
than native.
For a non-native layout it takes the mask of the default layout for the
same channel count with `Option.get`. `get_default` returns an
unspecified-order layout when FFmpeg has no default, which has no mask.

### Derived option entries shadow rather than replace

`avutil/avutil.ml:824-839`, `862-875`, `808-812`. **Narrowed** (by a
run).
`mk_audio_opts` and `mk_video_opts` use `Hashtbl.add`. A key already
present in the caller's table is then bound twice, and `mk_opts_array`
emits both pairs. The order is fixed: `Hashtbl.fold` presents the newer
binding first and `mk_opts_array` prepends, so the caller's pair precedes
the derived one in the array. Run (OCaml 5.5.0, a table with 42 keys,
copied, then `Hashtbl.add` on an existing key, folded the same way):
printed `caller` then `derived`. A consumer that inserts the pairs in
array order with an overwriting `av_dict_set` keeps the derived value
(`av_dict_set` semantics not re-read here). `string_of_opts` emits both
`key=value` forms.

## API gaps

### `Options.entry.values` is declared and never populated

`avutil/avutil.mli:451-458`. **Confirmed (second read)** with the defect
above. Either the field has a
meaning the code must implement, or it does not belong to the type.

### `Channel_layout.find`, `get_default`, `Sample_format.find_id`, `Pixel_format.find_id` document `Not_found`

`avutil/avutil.mli:169`, `189`, `217`, `307`. **Confirmed (second read)**
with the defect above. The implementation raises
`Error` or nothing. The contract a caller can rely on has to be picked.

### `Channel_layout.compare` is an equality test

`avutil/avutil.mli:166-167`. **Confirmed (second read):** the stub returns
`Val_bool(!ret)` and raises on a negative `av_channel_layout_compare`
(`avutil_stubs.c:489-495`). Its type is `t -> t -> bool` and it returns
`true` for equal layouts. The name and doc comment say "compare", and the
module therefore cannot be passed where an ordering `compare` is expected.

### `Channel_layout.layout` has no operation

`avutil/avutil.mli:155`. **Confirmed (second read):** no `.ml` or `.mli`
in the tree mentions `Channel_layout.layout`. The generated variant of named layouts is exported
as a type; nothing converts it to or from `t`.

### `get_native_id` deprecation is a floating attribute

`avutil/avutil.mli:197-200`. **Checked.** The alert is written as
`[@@@caml.alert ...]` after the item, not attached to it, and has no
effect: `get_native_id` is not deprecated for callers. Run: a standalone
`.mli` with the same nested signature and trailing floating attribute, and
a client calling `get_native_id`, compiled with `ocamlopt -w +a` (5.5.0)
with no alert. No code in the tree calls `get_native_id`.

### `Subtitle.pict.planes` admits arrays that can only be misread

`avutil/avutil.mli:403-410`. **Confirmed (second read)**
(`avutil_stubs.c:1365-1387`, `1526-1538`). Exactly four planes are read and produced; the
type is two unconstrained arrays.

### `Subtitle.content.pts` and `Subtitle.get_pts` overlap

`avutil/avutil.mli:425`, `430`. **Confirmed (second read)**
(`avutil_stubs.c:1424-1428`, `1451-1456`). Both return the same field; consistent
today.

### `Options` getters cover part of `ground`

`avutil/avutil.mli:495-505`. **Confirmed (second read)** against the
`.mli` and the `switch` at `avutil_stubs.c:1586-1714`. There is no getter for `` `Bool ``,
`` `Duration ``, `` `Color ``, `` `Binary ``, `` `UInt64 `` or `` `Flags ``
as such (the integer getters reach some of them through
`av_opt_get_int`). Recorded as a fact; whether it is a gap depends on
intended use.

### Hardware contexts and frames have no explicit release

`avutil/avutil.mli:543-565`, `33-70`. **Confirmed (second read):** the
only `av_buffer_unref` and `av_frame_free` on these values are in the
finalisers (`avutil_stubs.c:859-862`, `2071`). Device contexts, frame contexts and
frames are released by garbage collection only. A device context can hold
an OS device handle.

### `Time_format.t` has no operation in `Avutil`

`avutil/avutil.mli:120-123`. **Confirmed (second read).** The type is used by sibling libraries only;
the conversion lives in the C header.

## To verify

### `av_hwdevice_ctx_create` and the unused-options report

`avutil/avutil_stubs.c:2094-2106`, `avutil/avutil.ml:900-906`. **Read
only.**
From memory, `av_hwdevice_ctx_create` takes `AVDictionary *opts` by value
and does not remove the entries it uses. If so, the unused report always
lists every key and `filter_opts` never removes anything from the caller's
table for this call.

### `av_opt_next` on the address of a class pointer

`avutil/avutil_stubs.c:1808`, `1817`. **Settled (second read).**
Relies on `av_opt_next` reading only `*(const AVClass **)obj`. It does, at
`n7.1.5` and `n9.0.2` (`libavutil/opt.c`): the object is dereferenced once
to get the class and nothing else is read from it.

### Child classes of child classes

`avutil/avutil_stubs.c:1810-1819`. **Read only.**
`av_opt_child_class_iterate` is always called on the root class. Whether
any FFmpeg class has option-bearing grandchildren that this misses was not
checked.

### `Bool_val` on a boxed option

The `search_children` defect assumes `Bool_val(v)` is `Int_val(v)` on a
pointer, which is non-zero. True for the runtime headers installed here.
**Settled (second read):** OCaml 5.5.0 `caml/mlvalues.h` defines it as
`Int_val(x)`, or `(bool) Int_val(x)` under C23; both are true for a block
pointer.

### `expr_parse_and_eval` and logging

`avutil/avutil_stubs.c:2158-2159`. **Checked** in passing: three failing
evaluations produced no log message, consistent with
`log_offset = AV_LOG_MAX_OFFSET` silencing them.

### Padding around the last plane

The over-long chroma bigarray reads past the plane. Whether it also passes
the end of the `AVBufferRef` for every pixel format depends on FFmpeg's
buffer layout and padding in `av_frame_get_buffer`; not measured.
**Settled (second read)** for buffers from `av_frame_get_buffer`
(`libavutil/frame.c`, `get_video_buffer`). One buffer holds all planes,
each sized for `FFALIGN(height, 32)` rows, so a 4:2:0 chroma plane holds
`FFALIGN(height, 32) / 2` rows. At `n7.1.5` the bytes available from the
start of the last of three planes are its size plus `2 * plane_padding`;
at `n9.0.2` at most its size plus `2 * plane_padding + 4 * align`, with
`plane_padding` 32 or 64. For 64x48 `yuv420p` with `align = 32` that is
1088 and at most 1280 bytes against the 1536 exposed: the last bigarray
runs 448 bytes (respectively at least 256) past the end of the
`AVBufferRef`. Frames from decoders and filters
use other allocators and were not examined.

### Floating alert attribute

`avutil/avutil.mli:200`. Whether a floating `[@@@caml.alert deprecated]`
at the end of a nested signature has any effect on users of
`Channel_layout` was not tested. The library compiles and a program using
`Channel_layout.find` and `standard_layouts` built with no alert.
**Settled (checked):** see "`get_native_id` deprecation is a floating
attribute"; a call to the item itself raises no alert either.

### Comment on `Store_field` evaluation order

`avutil/avutil_stubs.c:130-132`. **Checked** against the installed
`caml/memory.h`: `Store_field` assigns the value to a temporary before
taking the field address. The stated hazard does not apply to this macro;
the many direct `Store_field(x, i, caml_copy_string(...))` uses in the file
are sound.
