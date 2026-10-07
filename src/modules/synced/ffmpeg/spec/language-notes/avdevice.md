# avdevice — language notes (Part B)

How `avdevice/avdevice_stubs.c` and `avdevice/avdevice.ml` implement
[../avdevice.md](../avdevice.md).

## Build

`avdevice/dune`: library `avdevice`, public name `ffmpeg-avdevice`,
`(libraries ffmpeg-av)`, one foreign stub `avdevice_stubs`. C flags and link
flags come from `../detect/avdevice_c_flags.sexp` and
`../detect/avdevice_c_library_flags.sexp`. Enabled when
`../detect/avdevice_available` reads `true`. An `install`-alias rule runs
`../detect/check.exe avdevice` unless
`LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true`.

The stub includes `av_stubs.h` and `avutil_stubs.h`. No generated enum
table is used.

## Externals

| OCaml                                     | C symbol                                      | Notes                                                                 |
| ----------------------------------------- | --------------------------------------------- | --------------------------------------------------------------------- |
| `init`                                    | `ocaml_avdevice_init`                         | declared `[@@noalloc]`; the stub still uses `CAMLparam0`/`CAMLreturn` |
| `get_audio_input_formats`                 | `ocaml_avdevice_get_audio_input_formats`      | returns an array                                                      |
| `get_video_input_formats`                 | `ocaml_avdevice_get_video_input_formats`      |                                                                       |
| `get_audio_output_formats`                | `ocaml_avdevice_get_audio_output_formats`     |                                                                       |
| `get_video_output_formats`                | `ocaml_avdevice_get_video_output_formats`     |                                                                       |
| `App_to_dev.control_message`              | `ocaml_avdevice_app_to_dev_control_message`   | one message                                                           |
| `Dev_to_app.set_control_message_callback` | `ocaml_avdevice_set_control_message_callback` |                                                                       |

## Initialisation

`avdevice.ml:3-9`: a `ref false` flag and a top-level
`if not !init_done then init (); init_done := true`. The public `init` is
the raw external and does not consult the flag.

## Enumeration helpers

`get_input_devices` / `get_output_devices` (`avdevice_stubs.c:23-82`) take a
function pointer to the `av_*_device_next` iterator, typed with
`avioformat_const` from `av_stubs.h`. Two passes; `caml_alloc_tuple(len)`
then `value_of_inputFormat` / `value_of_outputFormat` (from `av`) into a
rooted local and `Store_field`. The primitives return the helper's result
through `CAMLreturn`.

## Variant encoding

`App_to_dev.message` (`avdevice_stubs.c:96-136`): two C arrays indexed by
the OCaml representation.

- Constant constructors, by `Int_val`: `None`=0, `Pause`=1, `Play`=2,
  `Toggle_pause`=3, `Mute`=4, `Unmute`=5, `Toggle_mute`=6, `Get_volume`=7,
  `Get_mute`=8.
- Block constructors, by `Tag_val`: `Window_size`=0, `Window_repaint`=1,
  `Set_volume`=2. The rect constructors have four immediate fields;
  `Set_volume` has one boxed float read with `Double_val(Field(msg, 0))`.

No bounds check on either index.

`Dev_to_app.message` (`avdevice_stubs.c:150-215`): tags are `#define`d.

- Constant: `None`=0, `Prepare_window_buffer`=1, `Display_window_buffer`=2,
  `Destroy_window_buffer`=3, `Buffer_overflow`=4, `Buffer_underflow`=5.
- Block (one field each): `Create_window_buffer`=0, `Buffer_readable`=1,
  `Buffer_writable`=2, `Mute_state_changed`=3, `Volume_level_changed`=4.

`Buffer_readable`/`Buffer_writable` build `Some` as a 1-field block holding
`caml_copy_int64`. `Create_window_buffer` stores a `caml_alloc_tuple(4)`
directly as the constructor argument (see findings). `msg` is a
`CAMLlocal`, so an unrecognised type leaves it at `Val_unit`, which is the
representation of `None`.

## Seam with `av` (`av_stubs.h`)

- `AVFormatContext *ocaml_av_get_format_context(value *p_av)` — goes
  through `av`'s checked accessor, which raises through an OCaml callback on
  a closed container.
- `void ocaml_av_set_control_message_callback(value *p_av,
av_format_control_message c_callback, value *p_ocaml_callback)` — stores
  the closure in the container's C struct, registers it as a generational
  global root on first use and uses
  `caml_modify_generational_global_root` afterwards; sets
  `format_context->opaque` to the container struct and
  `format_context->control_message_cb`.
- `value *ocaml_av_get_control_message_callback(AVFormatContext *ctx)` —
  returns the address of that slot via `ctx->opaque`.

`av` removes the root when the container is closed.

## Callback mechanics

`c_control_message_callback` (`avdevice_stubs.c:225-234`) is the function
installed in FFmpeg: `ocaml_ffmpeg_register_thread()` (from `avutil`:
`caml_c_thread_register` plus a pthread key so the thread is unregistered
at exit), `caml_acquire_runtime_system()`, the worker,
`caml_release_runtime_system()`.

`ocaml_control_message_callback` (`avdevice_stubs.c:162-223`) is the worker:
`CAMLparam0`, `CAMLlocal3(msg, opt, res)`, builds the message,
`caml_callback_exn(*slot, msg)`, and on `Is_exception_result` extracts and
drops the exception and returns `AVERROR_UNKNOWN` via `CAMLreturn`.

## Runtime lock

- `ocaml_avdevice_app_to_dev_control_message`
  (`avdevice_stubs.c:138-142`): the message is decoded into C locals first;
  then `caml_release_runtime_system()`, then
  `ocaml_av_get_format_context(&_av)`, then the FFmpeg call, then
  re-acquire.
- `ocaml_avdevice_set_control_message_callback`
  (`avdevice_stubs.c:240-243`): `caml_release_runtime_system()`,
  `ocaml_av_set_control_message_callback(&_av, …, &_control_message_callback)`,
  re-acquire. Both `value`s are passed by address of the `CAMLparam`-rooted
  parameters.

See findings for what these two orderings imply.
