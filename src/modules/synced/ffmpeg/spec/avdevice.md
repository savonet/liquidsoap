# avdevice

Capture and playback devices, and the control-message channel between
application and device. It follows
[binding-contract.md](binding-contract.md); section numbers match.

## 1. Scope

`Avdevice` binds libavdevice: device registration, enumeration of device
input and output formats, and control messages.

It depends on `av`: a device is an `av` container opened with a device
format. From `av` it uses format values, `Av.open_input`,
`Av.open_output_format`, the container's native context and guard, and the
control-message closure of [avformat.md](avformat.md) §7.1.5. It uses
`avutil`'s error exception and thread registration.

It installs no C header and provides nothing to other libraries.

**Module initialisation** registers FFmpeg's devices with libavformat. A
program that links this library sees the device formats among libavformat's
formats without calling any function of it. It cannot fail.

## 2. Objects

None of its own. Devices are `av` containers; creation, ownership, states,
close and collection are [avformat.md](avformat.md) §2.1. Device formats are
`av` format values.

The only state this library adds to a container is the control-message
closure (§7).

Where the types do not protect (B3): `App_to_dev.control_messages` and
`Dev_to_app.set_control_message_callback` accept any container. On a
container that is not a device the first raises the error FFmpeg reports and
the second installs a closure that is never called.

## 3. Enumerations and constants

Both message types are ordinary OCaml variants with hand-written tables.

### 3.1 `App_to_dev.message` (OCaml to C)

| Constructor                   | C message type                 | Payload sent                                                                    |
| ----------------------------- | ------------------------------ | ------------------------------------------------------------------------------- |
| `None`                        | `AV_APP_TO_DEV_NONE`           | none                                                                            |
| `Window_size (x, y, w, h)`    | `AV_APP_TO_DEV_WINDOW_SIZE`    | the rectangle                                                                   |
| `Window_repaint (x, y, w, h)` | `AV_APP_TO_DEV_WINDOW_REPAINT` | the rectangle when `w > 0`; none when `w <= 0`, which asks for the whole window |
| `Pause`                       | `AV_APP_TO_DEV_PAUSE`          | none                                                                            |
| `Play`                        | `AV_APP_TO_DEV_PLAY`           | none                                                                            |
| `Toggle_pause`                | `AV_APP_TO_DEV_TOGGLE_PAUSE`   | none                                                                            |
| `Set_volume v`                | `AV_APP_TO_DEV_SET_VOLUME`     | the volume, a double                                                            |
| `Mute`                        | `AV_APP_TO_DEV_MUTE`           | none                                                                            |
| `Unmute`                      | `AV_APP_TO_DEV_UNMUTE`         | none                                                                            |
| `Toggle_mute`                 | `AV_APP_TO_DEV_TOGGLE_MUTE`    | none                                                                            |
| `Get_volume`                  | `AV_APP_TO_DEV_GET_VOLUME`     | none                                                                            |
| `Get_mute`                    | `AV_APP_TO_DEV_GET_MUTE`       | none                                                                            |

### 3.2 `Dev_to_app.message` (C to OCaml)

| C message type                        | Constructor                | Payload                                                           |
| ------------------------------------- | -------------------------- | ----------------------------------------------------------------- |
| `AV_DEV_TO_APP_NONE`                  | `None`                     |                                                                   |
| `AV_DEV_TO_APP_CREATE_WINDOW_BUFFER`  | `Create_window_buffer opt` | `Some (x, y, width, height)` of the rectangle; `None` without one |
| `AV_DEV_TO_APP_PREPARE_WINDOW_BUFFER` | `Prepare_window_buffer`    |                                                                   |
| `AV_DEV_TO_APP_DISPLAY_WINDOW_BUFFER` | `Display_window_buffer`    |                                                                   |
| `AV_DEV_TO_APP_DESTROY_WINDOW_BUFFER` | `Destroy_window_buffer`    |                                                                   |
| `AV_DEV_TO_APP_BUFFER_OVERFLOW`       | `Buffer_overflow`          |                                                                   |
| `AV_DEV_TO_APP_BUFFER_UNDERFLOW`      | `Buffer_underflow`         |                                                                   |
| `AV_DEV_TO_APP_BUFFER_READABLE`       | `Buffer_readable opt`      | `Some` of the 64-bit amount; `None` without one                   |
| `AV_DEV_TO_APP_BUFFER_WRITABLE`       | `Buffer_writable opt`      | the same                                                          |
| `AV_DEV_TO_APP_MUTE_STATE_CHANGED`    | `Mute_state_changed b`     | whether the device is muted                                       |
| `AV_DEV_TO_APP_VOLUME_LEVEL_CHANGED`  | `Volume_level_changed v`   | the volume, a double                                              |

- A message whose type needs a payload and that carries none is not
  delivered, except where the table gives `None`.
- A message of a type not in the table is not delivered.

## 4. Operations

### 4.1 Initialisation

```ocaml
val init : unit -> unit
```

Registers FFmpeg's devices. Module initialisation already does; calling it
again has no further effect. It exists so that a program can name something
from this library and thereby make the linker keep it.

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

Each list function returns the device formats of that kind and direction that
FFmpeg registers, in FFmpeg's order. The list is built on every call.

Each `get_default_*` function returns the first element of the matching list:
"default" means "first registered", and no system default is consulted. It
raises `Not_found` when the list is empty.

What is enumerated are device **formats** (`alsa`, `v4l2`), not the devices of
each format.

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

- **Lookup.** The format is the first of the matching list (§4.2) whose name
  is the given one. A format's name may be a comma-separated list of aliases;
  each alias matches. No match raises `Not_found`.
- **Named input**: `Av.open_input` with that format and an empty URL, no
  option and no interrupt function.
- **Named output**: `Av.open_output_format` with that format, `interleaved`
  and `opts` passed through.
- **Default** variants use the first format of the list and raise `Not_found`
  when the list is empty.

Errors of the `Av` operations propagate. The result is an ordinary `av`
container.

A caller that needs to name a particular device, or to pass options to an
input, calls `Av.open_input` or `Av.open_output` with a format from §4.2 and
the device as the URL.

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

Sends the messages to the device one by one, in list order
(`avdevice_app_to_dev_control_message`). The first failure raises the mapped
error and the remaining messages are not sent. A container that is not an
output device with a message handler fails with the error FFmpeg reports.

`Get_volume` and `Get_mute` have no result here: the device answers through
the device-to-application closure (§7), when one is installed.

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

Installs the closure that receives the device's messages (§7). A second call
replaces the closure. The closure is dropped when the container is released;
there is no other way to remove it.

## 5. Errors

| Raised | By  |
| ------ | --- |

| `Not_found` | the four `get_default_*_format` and the eight open functions, when no device format matches or none is registered |
| `Error e`, `e` mapped from an FFmpeg code | `App_to_dev.control_messages` |
| the state errors of the contract's §5.3 | `control_messages`, `set_control_message_callback` |
| whatever the `Av` open functions raise | the eight open functions |

## 6. Blocking and concurrency

- `App_to_dev.control_messages` releases the runtime lock around each message
  sent (M1, M4): a device may answer by calling the closure of §7 from inside
  the call. The container's native context is obtained, and its state
  checked, before the lock is released (M2).
- `Dev_to_app.set_control_message_callback` holds the lock throughout.
- Guards: both operations take the container's guard exclusively.
- Global state: FFmpeg's device registry.

## 7. Callbacks

### 7.1 Device-to-application messages

Trigger: the device sends a message
(`avdevice_dev_to_app_control_message`). This happens on the thread that is
inside an operation on the container, or on a thread the device owns, at any
time while the container is open.

The closure is called through the contract's §7.1 sequence, with the message
built per §3.2.

- The closure's result is not used.
- An exception is caught (C1); the device is told the message failed.
- Every `av` operation that can make a device send a message releases the
  runtime lock around the native call (M4): writes, reads, the header, the
  trailer, close, and `control_messages`.
- A message is delivered whether or not an operation holds the container's
  guard at that moment (C4).
- No message is delivered after the container's release completed (L8).

### 7.2 Functions the binding calls

None.

## 8. Data transfer

| Path                                                         | Copy or share                                               |
| ------------------------------------------------------------ | ----------------------------------------------------------- |
| device formats                                               | borrowed handles on FFmpeg's static formats                 |
| the rectangle and volume of an application-to-device message | copied into native storage that lives for the call          |
| device-to-application payloads                               | copied into fresh OCaml values before the closure is called |

No frame or packet passes through this library; device I/O goes through `av`.

## 9. Options

`?opts` of the four output-open functions is forwarded to
`Av.open_output_format` unchanged ([avformat.md](avformat.md) §9). The
input-open functions take no option.

## 10. Version-dependent behaviour

None in the binding. Which devices emit which messages depends on the FFmpeg
version and build.

## 11. Composite operations

- `get_default_*`: the head of the list.
- The eight open functions: lookup, then the `Av` open.
- `control_messages`: one send per message, in order.
