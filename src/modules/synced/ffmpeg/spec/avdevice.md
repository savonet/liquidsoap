# avdevice — as-built specification (Part A)

Mechanism notes are in [language-notes/avdevice.md](language-notes/avdevice.md).
Observations and judgement are in [findings/avdevice.md](findings/avdevice.md).

## 1. Scope

Binds `libavdevice`: device registration, enumeration of device
input/output formats, and the control-message channel between application
and device.

No minimum version is stated and there is no version test in this library.
Whether it is built is decided by the detection step.

Sibling dependencies:

- `av` (libavformat binding). avdevice adds no object of its own. It uses:
  - `Av`'s format values (`(input, _) format`, `(output, _) format`), built
    with `av`'s wrappers around `AVInputFormat *` / `AVOutputFormat *`
    (the input wrapper fails with ``Avutil.Error (`Failure "Empty input
format")`` on `NULL`);
  - `Av.Format.get_input_name` / `Av.Format.get_output_name`;
  - `Av.open_input` and `Av.open_output_format` to open devices;
  - `av`'s container object (`_ container`) as the device handle;
  - three C-level services of `av` (section 7.1): get the
    `AVFormatContext *` of a container (fails with
    ``Avutil.Error (`Failure "Container closed!")`` on a closed container),
    install a control-message callback on a container, and retrieve the
    callback slot from an `AVFormatContext *`.
- `avutil`: the `Avutil.Error` exception and error-code mapping, and the
  helper that registers a foreign thread with the OCaml runtime.

The library installs no C header.

## 2. Objects

Nothing of its own. Devices are `av` containers (`input container`,
`output container`); creation, ownership, close and garbage collection are
`av`'s. Device formats are `av` format values that point at FFmpeg's static
format descriptors.

The only state avdevice adds to a container is the control-message callback
installed through `av` (section 7).

## 3. Enumerations and constants

Both message types are ordinary OCaml variants. The mapping is positional
and hand-written.

### 3.1 `App_to_dev.message` (OCaml to C)

| Constructor                   | C message type                 | Payload sent                                                      |
| ----------------------------- | ------------------------------ | ----------------------------------------------------------------- |
| `None`                        | `AV_APP_TO_DEV_NONE`           | none (`NULL`, 0)                                                  |
| `Window_size (x, y, w, h)`    | `AV_APP_TO_DEV_WINDOW_SIZE`    | `AVDeviceRect {x, y, width, height}`, size `sizeof(AVDeviceRect)` |
| `Window_repaint (x, y, w, h)` | `AV_APP_TO_DEV_WINDOW_REPAINT` | the rect when `w > 0`; none (`NULL`, 0) when `w <= 0`             |
| `Pause`                       | `AV_APP_TO_DEV_PAUSE`          | none                                                              |
| `Play`                        | `AV_APP_TO_DEV_PLAY`           | none                                                              |
| `Toggle_pause`                | `AV_APP_TO_DEV_TOGGLE_PAUSE`   | none                                                              |
| `Set_volume v`                | `AV_APP_TO_DEV_SET_VOLUME`     | a `double`, size `sizeof(double)`                                 |
| `Mute`                        | `AV_APP_TO_DEV_MUTE`           | none                                                              |
| `Unmute`                      | `AV_APP_TO_DEV_UNMUTE`         | none                                                              |
| `Toggle_mute`                 | `AV_APP_TO_DEV_TOGGLE_MUTE`    | none                                                              |
| `Get_volume`                  | `AV_APP_TO_DEV_GET_VOLUME`     | none                                                              |
| `Get_mute`                    | `AV_APP_TO_DEV_GET_MUTE`       | none                                                              |

### 3.2 `Dev_to_app.message` (C to OCaml)

| C message type                        | Constructor                | Payload read from `data`                                                                                                                                     |
| ------------------------------------- | -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `AV_DEV_TO_APP_NONE`                  | `None`                     | —                                                                                                                                                            |
| `AV_DEV_TO_APP_CREATE_WINDOW_BUFFER`  | `Create_window_buffer opt` | `data` non-`NULL`: read as `AVDeviceRect`; the argument is built as the bare 4-tuple `(x, y, width, height)` with no `Some` around it. `data` `NULL`: `None` |
| `AV_DEV_TO_APP_PREPARE_WINDOW_BUFFER` | `Prepare_window_buffer`    | —                                                                                                                                                            |
| `AV_DEV_TO_APP_DISPLAY_WINDOW_BUFFER` | `Display_window_buffer`    | —                                                                                                                                                            |
| `AV_DEV_TO_APP_DESTROY_WINDOW_BUFFER` | `Destroy_window_buffer`    | —                                                                                                                                                            |
| `AV_DEV_TO_APP_BUFFER_OVERFLOW`       | `Buffer_overflow`          | —                                                                                                                                                            |
| `AV_DEV_TO_APP_BUFFER_UNDERFLOW`      | `Buffer_underflow`         | —                                                                                                                                                            |
| `AV_DEV_TO_APP_BUFFER_READABLE`       | `Buffer_readable opt`      | `data` non-`NULL`: `Some` of the `int64_t` it points to; `NULL`: `None`                                                                                      |
| `AV_DEV_TO_APP_BUFFER_WRITABLE`       | `Buffer_writable opt`      | same                                                                                                                                                         |
| `AV_DEV_TO_APP_MUTE_STATE_CHANGED`    | `Mute_state_changed b`     | `*(int *)data != 0`; `data` is dereferenced unconditionally                                                                                                  |
| `AV_DEV_TO_APP_VOLUME_LEVEL_CHANGED`  | `Volume_level_changed v`   | `*(double *)data`; dereferenced unconditionally                                                                                                              |
| any other value                       | `None`                     | —                                                                                                                                                            |

`data_size` is ignored.

## 4. Operations

### 4.1 Initialisation

```ocaml
val init : unit -> unit
```

Calls `avdevice_register_all()`. Module initialisation calls it once
already. Each explicit call calls `avdevice_register_all()` again. The
function exists so a program can force the library to be linked.

### 4.2 Device formats

```ocaml
val get_audio_input_formats : unit -> (input, audio) format list
val get_default_audio_input_format : unit -> (input, audio) format
val get_video_input_formats : unit -> (input, video) format list
val get_default_video_input_format : unit -> (input, video) format
val get_audio_output_formats : unit -> (output, audio) format list
val get_default_audio_output_format : unit -> (output, audio) format
val get_video_output_formats : unit -> (output, video) format list
val get_default_video_output_format : unit -> (output, video) format
```

Each list function iterates FFmpeg's device list twice (count, then fill),
starting from `NULL`, and returns the formats in FFmpeg's order:

| Function                   | Iterator                      |
| -------------------------- | ----------------------------- |
| `get_audio_input_formats`  | `av_input_audio_device_next`  |
| `get_video_input_formats`  | `av_input_video_device_next`  |
| `get_audio_output_formats` | `av_output_audio_device_next` |
| `get_video_output_formats` | `av_output_video_device_next` |

The list is rebuilt on every call; nothing is cached. Each element wraps the
static format pointer.

Each `get_default_*` function calls the matching list function and returns
its first element. It raises `Not_found` when the list is empty. "Default"
means "first registered"; no system default is queried.

What is enumerated are device _formats_ (for example `alsa`, `v4l2`), not
the devices of each format; `avdevice_list_devices` and related calls are
not bound.

### 4.3 Opening devices

```ocaml
val open_audio_input : string -> input container
val open_default_audio_input : unit -> input container
val open_video_input : string -> input container
val open_default_video_input : unit -> input container
val open_audio_output : ?interleaved:bool -> ?opts:opts -> string -> output container
val open_default_audio_output : ?interleaved:bool -> ?opts:opts -> unit -> output container
val open_video_output : ?interleaved:bool -> ?opts:opts -> string -> output container
val open_default_video_output : ?interleaved:bool -> ?opts:opts -> unit -> output container
```

The `string` argument is a device **format name**.

Named input (`open_audio_input name`, `open_video_input name`):

1. Get the matching format list (section 4.2).
2. Take the first format whose `Av.Format.get_input_name` equals `name`
   exactly (whole-string equality; a format whose name is a comma-separated
   list of aliases matches only on the full string).
3. None found: raise
   ``Avutil.Error (`Failure ("Input device not found : " ^ name))``.
4. `Av.open_input ~format ""` — the URL is the empty string, and no
   interrupt callback, options or stream configuration are passed.

Default input: `Av.open_input ~format:(get_default_…_input_format ()) ""`.
`Not_found` when no such device format is registered.

Named output (`open_audio_output`, `open_video_output`):

1. Same lookup with `Av.Format.get_output_name`; failure raises
   ``Avutil.Error (`Failure ("Output device not found : " ^ name))``.
2. `Av.open_output_format ?interleaved ?opts format`. `interleaved` and
   `opts` are passed through unchanged, so their defaults and the handling
   of unused options are `av`'s. No file name or URL is given.

Default output: `Av.open_output_format ?interleaved ?opts` on the first
format of the list; `Not_found` when the list is empty.

Errors from `Av.open_input` / `Av.open_output_format` propagate unchanged.
The returned container is an ordinary `av` container.

### 4.4 `App_to_dev`

```ocaml
type message =
  | None
  | Window_size of int * int * int * int
  | Window_repaint of int * int * int * int
  | Pause | Play | Toggle_pause
  | Set_volume of float
  | Mute | Unmute | Toggle_mute
  | Get_volume | Get_mute
val control_messages : message list -> _ container -> unit
```

`control_messages msgs device` sends the messages one by one in list order.
For each message:

1. Convert it to a message type and optional payload (section 3.1). The
   payload lives on the C stack for the duration of the call.
2. Release the runtime lock.
3. Obtain the container's `AVFormatContext *` from `av`.
4. `avdevice_app_to_dev_control_message(ctx, type, data, data_size)`.
5. Re-acquire the runtime lock.
6. A negative result raises `Avutil.Error` (for example the code FFmpeg
   returns when the device does not implement control messages). Remaining
   messages of the list are not sent.

A device answers `Get_volume` / `Get_mute` through the device-to-application
callback (section 7), if one is installed.

### 4.5 `Dev_to_app`

```ocaml
type message =
  | None
  | Create_window_buffer of (int * int * int * int) option
  | Prepare_window_buffer
  | Display_window_buffer
  | Destroy_window_buffer
  | Buffer_overflow
  | Buffer_underflow
  | Buffer_readable of Int64.t option
  | Buffer_writable of Int64.t option
  | Mute_state_changed of bool
  | Volume_level_changed of float
val set_control_message_callback : (message -> unit) -> _ container -> unit
```

`set_control_message_callback f device` asks `av` to install avdevice's C
trampoline as the container's `control_message_cb` and to store `f` as the
container's control-message closure (section 7). A second call replaces the
closure. There is no operation to remove the callback. The runtime lock is
released for the duration of the installation call.

## 5. Errors

| Exception                                                      | Raised by                                                                                                                  |
| -------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| ``Avutil.Error (`Failure "Input device not found : <name>")``  | `open_audio_input`, `open_video_input`                                                                                     |
| ``Avutil.Error (`Failure "Output device not found : <name>")`` | `open_audio_output`, `open_video_output`                                                                                   |
| `Not_found`                                                    | the four `get_default_*_format` and the four `open_default_*` when no device format of that kind is registered             |
| `Avutil.Error e` (mapped FFmpeg code)                          | `App_to_dev.control_messages` on a negative return                                                                         |
| ``Avutil.Error (`Failure "Container closed!")``                | `App_to_dev.control_messages`, `Dev_to_app.set_control_message_callback` on a closed container (raised by `av`'s accessor) |
| ``Avutil.Error (`Failure "Empty input format")``               | from `av`'s wrapper; not reachable from the iterators                                                                      |
| whatever `Av.open_input` / `Av.open_output_format` raise       | the eight open functions                                                                                                   |

An exception raised by the OCaml control-message callback is not an OCaml
error anywhere: it is discarded and reported to the device as
`AVERROR_UNKNOWN` (section 7).

## 6. Blocking and concurrency

- `init`, the four enumeration functions: runtime lock held.
- `App_to_dev.control_messages`: the lock is released around each
  `avdevice_app_to_dev_control_message` call and around fetching the format
  context from the container.
- `Dev_to_app.set_control_message_callback`: the lock is released around
  the installation call into `av`.
- The device-to-application trampoline registers the calling thread with
  the OCaml runtime (through `avutil`'s helper) and acquires the runtime
  lock itself; it must be entered without the lock.

Global state: FFmpeg's device registry. `avdevice_register_all()` runs at
module initialisation; the `.mli` states that `init` is not thread-safe. An
OCaml-side flag guards the module-initialisation call only.

## 7. Callbacks

### 7.1 What avdevice asks of `av`

- **Format context accessor**: given a container value, return its
  `AVFormatContext *`.
- **Install**: given a container value, a C function of type
  `av_format_control_message` and an OCaml closure: keep the closure alive
  for as long as the container is open (replacing any previous one), make
  the format context's `opaque` identify the container, and set the format
  context's `control_message_cb` to the C function.
- **Lookup**: given an `AVFormatContext *` inside a callback, return the
  location of the closure installed on its container.
- **Release**: when the container is closed, stop keeping the closure
  alive.

### 7.2 Device-to-application path

Trigger: device code calls the format context's `control_message_cb`
(through `avdevice_dev_to_app_control_message`). This can happen on the
thread that is inside an `av` or avdevice call on that container, or on a
thread the device owns.

Steps of the trampoline:

1. Register the current thread with the OCaml runtime if it is not already
   registered.
2. Acquire the runtime lock.
3. Build the OCaml message from `type` and `data` (section 3.2).
4. Look the closure up from the format context (section 7.1) and apply it
   to the message.
5. If the closure raised: discard the exception; the result is
   `AVERROR_UNKNOWN`. Otherwise the result is `0`. The closure's return
   value is not used.
6. Release the runtime lock and return the result to the device.

The closure runs synchronously inside the device's call. Because step 2
acquires the lock unconditionally, the device must invoke the callback only
from code that runs without the runtime lock.

Closure lifetime: kept alive by `av` from installation until replaced or
until the container is closed.

## 8. Data transfer

| Path                                                       | Copy or share                                                                                            |
| ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| Device formats                                             | wrap FFmpeg's static descriptor pointers; nothing copied                                                 |
| `Window_size` / `Window_repaint` rect, `Set_volume` double | copied from the OCaml value into C stack storage, passed by pointer for the call only                    |
| Dev-to-app payloads (rect, `int64_t`, `int`, `double`)     | read from `data` and copied into fresh OCaml values before the closure is called; `data` is not retained |

No frames or packets pass through this library; device I/O goes through
`av`.

## 9. Options

`?opts` of the four output-open functions is forwarded to
`Av.open_output_format` unchanged. The input-open functions accept no
options and pass none. avdevice has no option handling of its own.

## 10. Version-dependent behaviour

Nothing in this library. The const-qualification of format pointers in the
iterator signatures follows a macro provided by `av`'s header (non-const up
to libavformat 59.0.100, const above).

## 11. Logic on the OCaml side

- One-time `init` at module initialisation.
- `get_default_*`: head of the list, `Not_found` on empty.
- Device lookup by exact format name, with the two `Failure` messages of
  section 5.
- The eight open functions as compositions of lookup and `av`'s open
  functions (section 4.3).
- `control_messages`: `List.iter` over the per-message primitive.
- Arrays returned by the enumeration primitives are converted to lists.
