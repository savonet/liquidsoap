# avdevice — findings

Each finding in Defects, Asymmetries and API gaps ends with its verdict from an adversarial second read. Gaps are **read only**. Nothing was run.

## Defects

### `Create_window_buffer` payload is built without `Some`

`avdevice/avdevice_stubs.c:172-185`, type at `avdevice/avdevice.mli:99`.
With non-NULL `data`, the 4-tuple is stored directly as the constructor
argument. OCaml reads the argument as `(int*int*int*int) option`: it sees a
non-immediate, so `Some t`, and takes field 0 of the 4-tuple — the
immediate `x` — as the tuple `t`. Destructuring it dereferences an integer.
`Buffer_readable`/`Buffer_writable` (`:199-208`) build the option
correctly.

**confirmed (second read)** — Reach: the only emitter is `opengl_enc.c` at n7.1.5 (`AV_DEV_TO_APP_CREATE_WINDOW_BUFFER` with a non-NULL rect); n8.1.3 has no emitter of this message left.

### OCaml runtime used without the runtime lock in `set_control_message_callback`

`avdevice/avdevice_stubs.c:240-243`. Between release and acquire the stub
calls `av`'s installer, which reads both OCaml values, runs `av`'s checked
container accessor (it raises an OCaml exception through a registered
callback when the container is closed) and calls
`caml_register_generational_global_root` or
`caml_modify_generational_global_root`
(`av/av_stubs.c:329-343`, `:82-87`). None of that is permitted without the
lock; nothing in the call blocks, so there is no reason given for the
release.

**confirmed (second read)** — read independently a third time; `av/av_stubs.c:329-343` read here too: `Av_val` (which calls back into OCaml through `Fail` on a closed container), the read of `*p_ocaml_callback` and the global-root registration all sit between release and acquire.

### OCaml heap read, and possible raise, without the runtime lock in `control_message`

`avdevice/avdevice_stubs.c:138-139`. `ocaml_av_get_format_context(&_av)` is
called after `caml_release_runtime_system()`: it reads the custom block and,
on a closed container, raises from a thread that does not hold the lock.

**confirmed (second read)** — read independently a third time; `ocaml_av_get_format_context` (`av/av_stubs.c:174-176`) is `Av_val(*p_av)->format_context`, a custom-block read plus the `Fail` callback on a closed container.

### `open_default_*` and `get_default_*` raise `Not_found`, the `.mli` promises `Error`

`avdevice/avdevice.ml:11,17,23,29,35,54-72`,
`avdevice/avdevice.mli:39-41,47-49,56-59,66-69`. The named variants raise
``Avutil.Error (`Failure …)``; the default variants raise `Not_found` when no
device format is registered.

**narrowed** — holds for the four `open_default_*`: the `.mli` says "Raise Error if the device is not found" and `hd` raises `Not_found` before `Av.open_*` is entered. The four `get_default_*_format` document no exception at all (`avdevice.mli:14,20,26,32`), so for them the defect is an undocumented `Not_found`, not a broken promise.

### `.mli` says `init` "is done implicitly if you use any of the module's API"

`avdevice/avdevice.mli:6-9`, `avdevice/avdevice.ml:3-9`. It is done at
module initialisation, and the exported `init` bypasses the `init_done`
flag, so every explicit call re-runs `avdevice_register_all()`. The flag
never prevents anything: it is `false` the only time it is tested.

**narrowed** — the `.mli` sentence is true: using any value of the module links it and runs its initialiser, which is how the implicit call happens. What holds is the rest: `init_done` is dead (tested once, when it is `false`) and the exported `init` is the raw external, so each explicit call re-runs `avdevice_register_all()`. That re-run is harmless: it is two atomic pointer stores of the same static lists (`avpriv_register_devices`, `libavformat/allformats.c`).

### Exception from the OCaml callback is silently discarded

`avdevice/avdevice_stubs.c:216-221`. Not logged, not re-raised later;
reported to the device as `AVERROR_UNKNOWN`. The `.mli` says nothing about
exceptions in the callback.

**confirmed (second read)** — `res = Extract_exception(res)` is the last use of the exception (`:218-221`); nothing logs or stores it. The only emitter at n8.1.3 (`pulse_audio_enc.c`) ignores the callback's return value too, so `AVERROR_UNKNOWN` reaches nobody.

## Asymmetries

### Input devices take no options and no URL; output devices take options

`avdevice/avdevice.ml:52-72`. Inputs: `Av.open_input ~format ""` — fixed
empty URL, no `?opts`, no `?interrupt`. Outputs: `?interleaved ?opts`
forwarded, no file name. Most input device formats select the device and
its parameters through the URL and options (To verify).

**confirmed (second read)** — `Av.open_input` has `?interrupt` and `?opts` (`av/av.mli:62-70`) and the four input openers pass neither, with `""` as URL.

### Named lookup re-enumerates and compares whole names

`avdevice/avdevice.ml:37-51`. Exact equality with
`Av.Format.get_*_name`; FFmpeg format names can be comma-separated alias
lists (To verify for device formats), which then match only when the caller
passes the full list string.

**confirmed (second read)** — real for one device format: `video4linux2,v4l2` is the registered name of both the v4l2 input and output (`libavdevice/v4l2.c`, `v4l2enc.c`, n8.1.3), so `open_video_input "v4l2"` raises; no other device name contains a comma.

### No bounds check on message tables

`avdevice/avdevice_stubs.c:117,135`. Safe only while the OCaml variant and
the two C arrays stay in step; nothing ties them together at build time.

**confirmed (second read)** — compared the variant declarations with both C arrays and with the `*_TAG` defines of the receive path: constructor order and tags are in step as written.

## Gaps

### The trampoline assumes it is always entered without the runtime lock

`avdevice/avdevice_stubs.c:225-234`. It acquires unconditionally. This holds
only if every `av` operation that can make a device emit a message (read,
write, header/trailer, close, and `control_messages` itself) releases the
lock around the FFmpeg call. That is a requirement on `av` that neither
side states. A device that emits from inside a call made with the lock held
deadlocks.

### Callback closure lifetime depends on container close

`av/av_stubs.c:154-155,329-343`. The closure is a global root until the
container is closed by `av`; avdevice offers no removal. After close the C
`control_message_cb` pointer and `opaque` are whatever `av` leaves; a device
thread that still emits after close is not addressed here.

### `opaque` of the format context is claimed by the callback installation

`av/av_stubs.c:342`. Installing the callback sets `format_context->opaque`
to the container struct. avdevice relies on nobody else using `opaque`.

### Unknown device message types are delivered as `None`

`avdevice/avdevice_stubs.c:166-216`. `msg` keeps its initial value. A
rewrite that initialises differently changes what the callback sees.

### `data_size` is ignored on the receive path

`avdevice/avdevice_stubs.c:164`. Payload size is trusted from the message
type.

### `init` is `[@@noalloc]` over a stub that sets up a local-roots frame

`avdevice/avdevice.ml:3`, `avdevice/avdevice_stubs.c:16-21`. Harmless as
written (no allocation, no raise).

## API gaps

### No way to select a device or pass options when opening an input

`avdevice/avdevice.mli:35-49`. See the first asymmetry. The four input
functions can only open "format X with URL `""`".

**confirmed (second read)** — see the first asymmetry.

### No way to give an output device a target name

`avdevice/avdevice.mli:51-69`. Same for outputs: `Av.open_output_format`
takes a format only.

**confirmed (second read)** — `ocaml_av_open_output_format` calls `open_output(format, NULL, NULL, ...)` (`av/av_stubs.c:1783`).

### The control-message callback cannot be removed

`avdevice/avdevice.mli:110-112`. Set and replace exist; clear does not.

**confirmed (second read)** — the only writer of the root is `ocaml_av_set_control_message_callback`; removal happens only at container close.

### `Get_volume` / `Get_mute` have no synchronous result

`avdevice/avdevice.mli:74-91`. `control_messages` returns `unit`; the answer
arrives, if at all, as `Volume_level_changed` / `Mute_state_changed` on the
callback. Nothing in the `.mli` says so.

**confirmed (second read)** — `pulse_audio_enc.c` handles both by invalidating its cached value and calling `pulse_update_sink_input_info`, which reports through `avdevice_dev_to_app_control_message`.

### Devices of a format cannot be listed

`avdevice/avdevice.mli:11-33`. Only device formats are enumerated;
"default" is "first registered format".

**confirmed (second read)** — no reference to `avdevice_list_devices`, `avdevice_list_input_sources` or `avdevice_list_output_sinks` in `avdevice/` or `av/`.

### `control_messages` accepts any container

`avdevice/avdevice.mli:91,112`. `_ container` admits containers that are not
devices; the call can then only fail (FFmpeg returns an error when the
format has no control-message handler — To verify).

**confirmed (second read)** — `avdevice_app_to_dev_control_message` returns `AVERROR(ENOSYS)` when `s->oformat` is NULL or has no `control_message` (`libavdevice/avdevice.c`), so on every input container and every non-device output the call can only raise.

## To verify

- `avdevice_app_to_dev_control_message` returns `AVERROR(ENOSYS)` when the
  output format lacks `control_message`, and is meaningful only for output
  devices.
- Which FFmpeg devices call `avdevice_dev_to_app_control_message`, and from
  which thread (caller's thread inside `write_packet` /
  `control_message`, or a device thread).
- The payload conventions of section 3.2 of the spec: `AVDeviceRect` or NULL
  for `CREATE_WINDOW_BUFFER`, `int64_t` or NULL for `BUFFER_READABLE` /
  `BUFFER_WRITABLE`, `int` for `MUTE_STATE_CHANGED`, `double` for
  `VOLUME_LEVEL_CHANGED`; `WINDOW_REPAINT` with NULL meaning "whole
  window".
- Input device formats that work with an empty URL.
- Whether `avdevice_register_all()` is safe to call repeatedly on the
  targeted versions.
- Device format names containing commas.

Settled in verification (FFmpeg read at n8.1.3 unless noted):

- `avdevice_app_to_dev_control_message`: `AVERROR(ENOSYS)` without an output
  format that has `control_message`; output devices only.
- Emitters of `avdevice_dev_to_app_control_message`: `pulse_audio_enc.c`
  only (n7.1.5 also `opengl_enc.c`). Pulse emits from its threaded-mainloop
  callbacks and, for `BUFFER_WRITABLE`, from `write_packet` on the caller's
  thread.
- Payloads as documented in `avdevice.h`: `AVDeviceRect` for
  `CREATE_WINDOW_BUFFER`, `int64_t` for `BUFFER_READABLE`/`BUFFER_WRITABLE`,
  `int` for `MUTE_STATE_CHANGED`, `double` for `VOLUME_LEVEL_CHANGED`, NULL
  for the others. The header documents no NULL case for the typed payloads.
- `avdevice_register_all()` is safe to repeat (two atomic stores).
- Device format names with commas: `video4linux2,v4l2` only.

## Refuted

### `Mute_state_changed` / `Volume_level_changed` dereference `data` unchecked

`avdevice/avdevice_stubs.c:209-214`. The three other payload-carrying
messages test `data` for NULL.

**refuted** — `libavdevice/avdevice.h` documents `data` as `int` and `double` for these two messages with no NULL case, and the only emitter (`pulse_audio_enc.c:107,113`) passes the address of a live variable. The NULL tests on the other messages are extra caution, so this is an asymmetry inside the callback and not a defect.
