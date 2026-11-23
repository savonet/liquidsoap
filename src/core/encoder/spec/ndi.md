# `%ndi` encoder and `output.ndi` (as built)

`%ndi` is a format without an encoder: it exists to give `output.ndi` a
frame type, and its registered encoder only raises. `output.ndi` never asks
the seam for a factory; it reads the format value and sends raw frames to
the NDI SDK itself. This file describes both faces, since `output.ndi` is
the only caller. The shared seam is [encoder.md](encoder.md); the output
contract is [output.md](../../outputs/spec/output.md).

`output.ndi` is flagged experimental and is built only when the `ndi`
binding (loaded at run time with ctypes from a user-supplied library file)
is available.

Part A answers the encoder skeleton (§1–§14), then the caller skeleton for
`output.ndi` (§C1–§C8).

## 1. Format and options

Syntax: `%ndi(...)` with sub-encoders only:

| Parameter     | Effect    |
| ------------- | --------- |
| `%audio`      | audio on  |
| `%audio.none` | audio off |
| `%video`      | video on  |
| `%video.none` | video off |

Both default to on: `%ndi(%audio)` still carries video
(prints `%ndi(%audio,%video)`); only `.none` removes a track.

Errors (kind `encoder`):

- a labelled parameter: "Invalid parameter!" at the value's position;
- an anonymous parameter or a sub-encoder with arguments:
  "Invalid %%ndi encoder parameter!" (the doubled `%` is printed as is);
- both tracks off: the script-level type computation raises
  "Invalid %%ndi encoder parameter!" first, so the value-level message
  "%%ndi encoder needs at least one audio or video field!" is never seen.

Type: with audio, one `audio` PCM track whose channel count is
`frame.audio.channels` (forced when the type is computed, unlike the other
encoders which hard-code 2); with video, one `video` track in the default
video format.

Printed as `%ndi(%audio,%video)`, `%ndi(%audio,%video.none)`, …
Extension: none (the format-level query raises "not found"); MIME
`application/octet-stream`; no bitrate; Icecast info all empty and no
Icecast content type.

## 2. Factory and instance creation

The factory always succeeds and returns an instance whose every call except
`bitrate` and `restart_offset` raises a runtime error, kind `invalid`,
message "The NDI encoder can only be used with `output.ndi`!", at the
format's position. Any output other than `output.ndi` given `%ndi` therefore
evaluates, starts, and fails on its first call into the encoder (observed
with `output.file`: the clock fails after 0 ticks and the script exits with
status 1).

## 3. `encode`

Raises (§2).

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Raises (§2).

## 6. `header`

Raises (§2).

## 7. `restart_offset`

"none".

## 8. `bitrate`

"none".

## 9. `stop`

Raises (§2).

## 10. Bytes produced

None. The format never produces bytes; `output.ndi` hands frames to the
SDK (§C4).

## 11. Concurrency and failures

See §2. Nothing else.

## 12. Segmenting (HLS) face

Nothing.

## 13. Where the bytes go

There are no bytes. The encoder seam is bypassed entirely: the "encoding"
(conversion to the SDK's frame structures) lives in the output, because the
destination (the NDI sender) consumes typed media frames, not a byte
stream. The format value is used only for its two booleans and its type.

## 14. Tests

None in `tests/` (no NDI library in CI).

---

## C1. Formats accepted and how the factory is obtained

`output.ndi(library_file, ?name, ?groups, ?self_sync=false, format,
source)` plus the common output parameters. The format must be an `%ndi`
value; anything else raises at evaluation "Only the %ndi encoder is allowed
for `output.ndi`!" (kind `invalid`). No factory is obtained.

At evaluation: `library_file` is checked readable, then the SDK is
initialised from it; "Invalid ndi library" if not found; "Ndi already
initialized with a different library: <file>" if another file was loaded
earlier in the process (the SDK is process-wide).

At construction the output reads `frame.audio.samplerate`,
`frame.video.framerate`, `frame.video.width/height` and the main tick rate
once.

## C2. Instance lifecycle

The "instance" is an NDI sender. It is created lazily on start or on the
first frame sent, with `name`, `groups` and the clock flags:

| `self_sync` | audio | video | SDK clocks on |
| ----------- | ----- | ----- | ------------- |
| false       | any   | any   | nothing       |
| true        | any   | yes   | video         |
| true        | yes   | no    | audio         |

Creating the sender also declares the output a self-paced synchronisation
source named `ndi` for its clock. Stop destroys the sender and withdraws
the sync source. A restart creates a new sender, with its position (§C3)
back to 0. `reset` does nothing.

`self_sync` is reported as the output's dynamic self-sync when true, and
as the upstream source's when false.

## C3. Call sequence

Per frame sent: if audio, send one audio frame; if video, send one video
frame per image in the frame; then advance the sender position by the
frame's length in main ticks. Metadata, track marks and `header`/`stop`
have no counterpart.

Timecodes are in 100 ns units: `position × (10 000 000 / main_rate)`, the
division being an integer division computed once. Each image's timecode
adds its offset in the frame (main ticks) times the same factor.

## C4. The bytes

- **Audio**: the frame's PCM, all channels, copied into a fresh float32
  planar buffer (`samples × channels`), channel stride `samples × 4`
  bytes, sample rate `frame.audio.samplerate`, timecode as above, no
  metadata, no timestamp.
- **Video**: each image is cropped or padded to the frame video size
  (viewport change, not scaled), rendered opaque, and its Y, U, V planes
  copied back to back into a fresh byte buffer; sent as I420 with the Y
  stride as line stride, `xres`/`yres` the frame size, frame rate
  `video_rate/1`, progressive, aspect ratio unset, no metadata.

Each send copies; nothing is buffered by the output.

## C5. Reopen, reconnect, append, file naming

Nothing beyond C2.

## C6. Failures

SDK errors are not checked by the output. Initialisation errors are §C1.
Exceptions out of a send propagate to the clock like any output's.

## C7. Threads and locks

All sends happen on the clock's thread. With `self_sync=true`, the SDK's
clocked send is expected to block to pace the stream (the output is a
self-paced sync source); see [findings](findings/simple-encoders.md#to-verify).

## C8. Tests

None. A harness needs an SDK double recording frames, timecodes and the
clock flags.
