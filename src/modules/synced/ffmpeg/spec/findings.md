# Findings

What reading the code this closely surfaced. The as-built documents describe
the behaviour without judging it; the judgement is here.

The detail lives in one file per subsystem. This file ranks what matters most
across them, lists the findings that belong to no single subsystem, and says
what was never run.

| Subsystem           | File                                             |
| ------------------- | ------------------------------------------------ |
| `avutil`            | [findings/avutil.md](findings/avutil.md)         |
| `avcodec`           | [findings/avcodec.md](findings/avcodec.md)       |
| `av`                | [findings/avformat.md](findings/avformat.md)     |
| `avfilter`          | [findings/avfilter.md](findings/avfilter.md)     |
| `avdevice`          | [findings/avdevice.md](findings/avdevice.md)     |
| `swresample`        | [findings/swresample.md](findings/swresample.md) |
| `swscale`           | [findings/swscale.md](findings/swscale.md)       |
| Build and generator | [findings/build.md](findings/build.md)           |
| Tests               | [findings/tests.md](findings/tests.md)           |

## How to read the marks

Every finding carries one mark:

- **checked** — a program was run and its output is quoted.
- **confirmed (second read)** — a second reader, told to refute it, read the
  code and FFmpeg's source at the 7.1.5, 8.1.3 and 9.0.2 tags and could not.
- **narrowed** / **worse than reported** — the second reader changed its reach.
- **found in verification** — new, from the second read; itself read once.
- **read only** — read once. A suspect.
- **refuted** — kept in a final section of its file, with the reason.

The second read covered Defects and API gaps in every file, and Asymmetries in
most. Gaps and To verify entries are mostly **read only**.

Programs were run against FFmpeg's development head (libavutil 61.7.100,
libavformat 63.7.100) with OCaml 5.5.0 on Linux aarch64.

## 1. Memory safety

Each of these corrupts memory or crashes from well-typed OCaml code.

| Finding                                                                                                                        | Where        | Mark                                                    |
| ------------------------------------------------------------------------------------------------------------------------------ | ------------ | ------------------------------------------------------- |
| A decoder that fails to open leaves a freed stream record in the container's table; the next read or the close uses it.        | `av` D1      | checked: segfault                                       |
| Opening an output with an interrupt function and an unguessable format leaves a garbage-collector root inside freed memory.    | `av` D2      | checked: segfault at the next major collection          |
| A rejected format option on output open frees the container record twice.                                                      | `av` D3      | checked: double free abort                              |
| The container stream time-base getter skips the closed check.                                                                  | `av` D8      | checked: segfault                                       |
| `get_frame_size` is typeable on streams with no encoder.                                                                       | `av` P2      | checked: segfault                                       |
| `open_output_format` on a file-based format passes a null URL.                                                                 | `av` P5      | checked: segfault                                       |
| `reopen_output_stream` then `close` on a file output.                                                                          | `av` P1      | checked: invalid free abort; buffered bytes lost        |
| A failed stream creation leaves a dangling table entry.                                                                        | `av` D16     | confirmed                                               |
| `write_packet` reads an OCaml value with the runtime lock released.                                                            | `av` D10     | confirmed                                               |
| Subtitle creation and content stubs hold unrooted OCaml values across allocations.                                             | `avutil`     | checked: segfault; abort in minor collection            |
| Video planes are exposed with the luma height for every plane, so chroma bigarrays extend past their buffer.                   | `avutil`     | checked; overrun sized from FFmpeg source               |
| Bigarrays from a frame visit do not keep the frame alive.                                                                      | `avutil`     | read only                                               |
| The option getter passes an uninitialised channel layout to FFmpeg, which may free a garbage pointer.                          | `avutil`     | found in verification                                   |
| An OCaml string is passed to FFmpeg with the runtime lock released when creating a hardware device.                            | `avutil`     | confirmed                                               |
| A failed graph parse frees every filter of the graph while the OCaml handles remain.                                           | `avfilter`   | found in verification, from FFmpeg source at three tags |
| An attached filter is a public record with a hidden trailing field the stubs dereference; rebuilding the record misleads them. | `avfilter`   | confirmed                                               |
| A filter context does not keep its graph alive.                                                                                | `avfilter`   | confirmed                                               |
| The `Create_window_buffer` device message carries its tuple where an option belongs.                                           | `avdevice`   | confirmed twice                                         |
| The control-message stubs touch OCaml state, and can raise, with the runtime lock released.                                    | `avdevice`   | confirmed twice                                         |
| `convert ~offset` on interleaved bytes and float arrays reads past the input and past the scratch buffer.                      | `swresample` | narrowed: bytes return correct data but over-read       |
| `Swscale.scale` indexes four-slot tables by the caller's array length.                                                         | `swscale`    | confirmed                                               |
| The data-kind module signatures of `swresample` and `swscale` are open: a user module that misdescribes its type is trusted.   | both         | confirmed                                               |

## 2. Lost or wrong data

| Finding                                                                                                                                         | Where               | Mark                                                                |
| ----------------------------------------------------------------------------------------------------------------------------------------------- | ------------------- | ------------------------------------------------------------------- |
| `Swresample.flush` never drains: the resampler's tail is unreachable.                                                                           | `swresample`        | checked: 17 of 48000 samples never returned                         |
| `convert ~offset`: wrong samples for interleaved float arrays, a byte offset for bigarrays, an exception for planar bytes and planar bigarrays. | `swresample`        | checked                                                             |
| A typed module's sample format silently replaces the explicit argument, so `from_codec` ignores the codec's format.                             | `swresample`        | confirmed                                                           |
| Codec enumeration drops every codec whose identifier has a negative variant representation.                                                     | `avcodec`           | checked: 47 of 280 video decoders, 93 of 223 audio decoders missing |
| Bitstream filter options are never applied.                                                                                                     | `avcodec`           | checked, and confirmed from FFmpeg source                           |
| `?frame_rate` of the video encoder has no effect; the derived `r`, `channel_layout` and `sample_fmt` keys match no option and vanish.           | `avcodec`           | worse than reported, at all three tags                              |
| Hardware upload and download copy no frame properties: timestamps are lost.                                                                     | `avcodec`, `av` G10 | confirmed (second read)                                             |
| Value-to-identifier conversion returns a range marker for the first codec of each family.                                                       | build               | narrowed: one live case, `pcm_rechunk`                              |
| Enum members declared after a `_NB` marker are cut from their table; converting them raises.                                                    | build               | worse than reported: affects 8.1 and 9.0                            |
| `SWR_DITHER_NONE` is absent from the dither table.                                                                                              | build               | checked                                                             |
| Filter arguments are rendered in reverse order, and FFmpeg rejects a positional value after a `key=value` pair.                                 | `avfilter`          | confirmed from FFmpeg source                                        |
| The array-separator lookup raises for every option that uses the default separator.                                                             | `avfilter`          | confirmed; hits the buffer sinks from 8.1                           |
| A short count from a custom write function is taken as a full write.                                                                            | `av` G4             | narrowed                                                            |
| `tell` truncates the position to 32 bits.                                                                                                       | `av` D9             | confirmed                                                           |
| `codec_attr` returns nothing for HEVC once it has parsed its parameter set.                                                                     | `av` D11            | confirmed                                                           |
| A failed encoder-stream creation leaves the stream in the container; `close` then raises on every call.                                         | `av` G1, P4         | checked on three muxers                                             |
| Log messages of one batch arrive newest first.                                                                                                  | `avutil`            | checked                                                             |
| A `set_callback` issued right after `clear_callback` is lost; an exception in the callback stops delivery for good.                             | `avutil`            | checked; narrowed                                                   |
| `~search_children:false` behaves as `true`.                                                                                                     | `avutil`            | confirmed                                                           |
| Option constants are never reported: `values` is always empty.                                                                                  | `avutil`            | confirmed                                                           |
| `` `Child_consts `` is never reported: the stub tests a macro name that does not exist.                                                         | `avutil`            | checked against the installed header                                |
| `Swscale.scale`'s last argument is a byte offset, documented as a row.                                                                          | `swscale`           | checked                                                             |

## 3. API gaps

The interface is kept as it is unless a logical gap is identified. These are
the candidates, grouped by kind. Each is detailed in its subsystem's file.

**A state with no way out, or a resource with no release**

- `av`: `reopen_output_stream` (P1); a stream after a failed creation (P4).
- `avcodec`: no reset after flush, so a decoder cannot be reused across a
  seek; no release operation for decoders, encoders, filters, packets or
  parameters.
- `avfilter`: a graph cannot be released explicitly; nothing follows
  `` `Flush `` on a source.
- `avutil`: hardware contexts and frames have no explicit release.
- `avdevice`: the control-message callback cannot be removed.
- `swresample`: no working drain.

**A type that admits a call that can only fail or misbehave**

- `av`: `get_frame_size` on a stream with no encoder (P2); `write_frame` on a
  subtitle stream has no constructible argument (P3); `open_output_format` on
  a file-based format (P5).
- `avfilter`: `launch` after `launch`; an attached filter rebuilt as a record;
  a context that outlives its graph.
- `avdevice`: `control_messages` accepts any container.
- `swresample`: `convert ~offset` on frames; `Bytes` with a planar format; the
  open `AudioData` signature.
- `swscale`: the open `VideoData` signature; frame input of any geometry.
- `avutil`: `Subtitle.pict.planes` admits shapes that can only be misread.

**Something declared and not provided, or the reverse**

- `avutil`: `Options.entry.values`; `Channel_layout.layout` and
  `Time_format.t` have no operation; the deprecation of `get_native_id` is a
  floating attribute with no effect.
- `avcodec`: subtitle and data decoders and encoders have types and no
  constructor; `Unknown` has no descriptor.
- `avfilter`: `parse_node.node_args` has no effect; buffer sources and sinks
  are not reachable through `find`.
- build: no constructor for "no dithering"; range markers are public codec
  identifiers.

**A missing counterpart**

- `avcodec`: no `Packet.set_flags`; decoders take neither options nor a
  hardware context where encoders take both; `capabilities` is typed for
  encoders only.
- `avdevice`: inputs take no options and no device name; outputs no target
  name; `Get_volume` and `Get_mute` have no synchronous result; devices of a
  format cannot be listed.
- `avutil`: option getters cover part of the option value type.

**Two operations that overlap with different semantics**

- `avcodec`: `Video.get_sample_aspect_ratio` and `Video.get_pixel_aspect`;
  `Avcodec.name` and the per-kind `get_name`; `Packet.content` and
  `Packet.to_bytes`.
- `avutil`: `Subtitle.content.pts` and `Subtitle.get_pts`;
  `Channel_layout.compare` is an equality test.
- `swresample`: two sources for the sample format.
- `swscale`: the plain scaler and the bigarray-to-bigarray converter.

**Values an encoding cannot represent**

- `avcodec`: packet duration 0 and position −1 read as "unset".

**The interface does not fix the enumerations**

The interface aliases generated types, so the set of constructors of every
enumeration is whatever the FFmpeg headers present at build time declare.

## 4. Documentation that contradicts the code

Interface comments that state something the code does not do. The interface
is frozen, so each is either a comment to correct or a behaviour to change.

- `avutil`: four comments promise `Not_found` where the code raises `Error`
  or nothing (three checked).
- `av`: a raising I/O callback gives `` `Other ``, documented as
  `` `Unknown `` (D12); `read_input` with no selection consumes the input,
  documented as reading all streams (D13); further mismatches in D14 and D17.
- `avcodec`, `avfilter`, `avdevice`: further entries in each file.
- `swscale`: the `scale` offset.

## 5. Findings that belong to no single subsystem

### Version conditionals

- **Eleven conditionals are dead** ([compatibility.md](compatibility.md) §3).
  Seven test a version below the detection bound: **checked** against the
  version headers at each release tag. Four test a macro that FFmpeg defines
  throughout the range: **checked** in the headers at 5.1 and 9.0.
- **One conditional is never true** on any version: the `` `Child_consts ``
  flag. **checked**.
- **The stated bound is untested.** The README says FFmpeg 5.1; CI builds 7.1
  and 8.0.1. On 5.1.10 the bindings compile and the first test step fails on
  an assumption about the AAC encoder's capabilities. **checked**.
- **The detection bounds are not the versions of any release.** They fall
  between 5.0 and 5.1. **checked** from the tagged headers.
- **Frame duration is silently unavailable on 5.1** (libavutil 57.28.100 is
  below the 57.30.100 threshold): the setter does nothing and the getter
  returns nothing. **read only**.

### Cross-compilation

Nothing in [cross-compilation.md](cross-compilation.md) was run: this machine
has no Windows toolchain.

- **Gap: variant hashes are computed by the build machine's runtime.** The
  generated constants are right for a 64-bit target built on a 64-bit
  machine. **read only**.
- **Gap: the context and pkg-config selection is written twice**, in the
  detector and in the include-path discoverer, and the two must agree.
  **confirmed (second read)**, in [findings/build.md](findings/build.md).
- **Gap: the include-path discovery is tied to the build machine's context.**
  Its result is compiled into the generator, and the generator that runs
  under cross-compilation is the build machine's. `LIQUIDSOAP_DUNE_TARGET` is
  what redirects it to the target's pkg-config; the code does not say so, and
  the opam packages do not set it. **reasoned from the build rules, not run**.
- **Asymmetry: the two invocations select pkg-config differently.** The opam
  packages rely on the ambient `PKG_CONFIG_PATH`; the embedded build unsets it
  and uses the per-context variables. **read only**.
- **Gap: the opam-cross-windows packages lag the tree** (1.2.1 against 1.4.0)
  and carry `strndup` patches the tree no longer needs. **checked** against
  the package repository.
- **To verify:** the value of `%{os_type}` and the exact form of `%{cc}` in
  the `default.windows` context; that the opam recipes install correctly with
  no install step of their own; that the static link flags pkg-config returns
  are complete; that the stubs build against winpthreads.

### OCaml versions

- **Gap: the stated bound is untested.** Metadata says OCaml 4.12; CI runs
  4.14 and 5.3. **read only**.
- **To verify:** the OCaml versions below which each of the four runtime
  provisions in [compatibility.md](compatibility.md) §5 is needed. They are
  stated from memory.

### Concurrency

- **Gap, in every library:** no object is protected against concurrent use,
  and most operations release the runtime lock mid-way. Two OCaml threads on
  one container, codec, graph or converter race inside FFmpeg. No interface
  comment says so. **read only**.

### From the project's history

[known-complexity.md](known-complexity.md) lists 61 pitfalls mined from the
history. 42 are covered by a stated rule and 14 are findings already listed in
the subsystem files. Five are not covered by the specification. All **read
only**.

- **Gap: no rule on holding OCaml values across an allocation.** The
  specification says which objects exist and who owns them. It states no rule
  for values a stub holds while it allocates or calls back into OCaml, which
  is the most repeated crash in the history.
- **Gap: no rule on what may be stored in an OCaml block or passed as a native
  object.** Integers stored in blocks are tagged; a reader of a native object
  receives the native object.
- **Gap: no rule on allocator families.** Nothing states that what crosses the
  boundary is allocated by FFmpeg's allocator and released by one call per
  object kind.
- **Gap: text FFmpeg may return as null.** [avfilter.md](avfilter.md) §2
  describes pad names as copied unconditionally. An upstream issue reports a
  crash at module load from a null name.
- **Gap: a copied stream has no frame rate.** [avformat.md](avformat.md) §4.4
  does not say that copying a stream leaves its average frame rate unset.

## 6. What was never run

- Anything on Windows or under cross-compilation.
- Anything on macOS.
- FFmpeg 7.0, 6.x and 5.0. FFmpeg 5.1 beyond compilation and the first test
  step.
- OCaml older than 5.5.0.
- Hardware encoding, decoding, upload and download: no device was available,
  and the two hardware test steps pass on every outcome that is not a crash.
- Any capture or playback device, and so the whole of `avdevice` beyond
  reading.
- The `soxr` resampling engine and PNG decoding, absent from the FFmpeg
  builds used.
- liquidsoap's own use of the bindings.
- Most Gaps and every To verify entry not marked otherwise.
