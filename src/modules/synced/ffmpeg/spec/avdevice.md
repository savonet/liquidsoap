# avdevice

Registration of FFmpeg's capture and playback devices. It follows
[binding-contract.md](binding-contract.md); section numbers match.

## 1. Scope

`Avdevice` binds libavdevice for one purpose: making FFmpeg's devices
available to `av`.

A device is an `av` container opened with a device format. Once this library
is linked, the device formats are among the formats `av` finds
([avformat.md](avformat.md) §4.2), and a device is opened with `Av.open_input`
or `Av.open_output`, the device as the URL, or with `Av.open_output_format`.

It depends on `av`. It installs no C header and provides nothing to other
libraries.

**Module initialisation** registers FFmpeg's devices with libavformat. A
program that links this library sees the device formats among libavformat's
formats without calling any function of it. It cannot fail.

## 2. Objects

None. Devices are `av` containers; creation, ownership, states, close and
collection are [avformat.md](avformat.md) §2.1. Device formats are `av` format
values.

## 3. Enumerations and constants

None.

## 4. Operations

```ocaml
val init : unit -> unit
```

Registers FFmpeg's devices. Module initialisation already does; calling it
again has no further effect.

It exists so that a program can name something from this library and thereby
make the linker keep it.

## 5. Errors

None. `init` raises nothing.

## 6. Blocking and concurrency

`init` holds the runtime lock, takes no guard, and may be called from any
thread at any time. Global state: FFmpeg's device registry.

## 7. Callbacks

None of either kind.

## 8. Data transfer

None. Device I/O goes through `av`.

## 9. Options

None. A device takes its options through the `av` open functions
([avformat.md](avformat.md) §9).

## 10. Version-dependent behaviour

None in the binding. Which devices exist depends on the FFmpeg version, build
and platform.

## 11. Composite operations

None.
