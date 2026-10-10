# Side data

What spans the libraries about side data: the two families, what carries
them, the raw entry, and the three levels of the interface. The signatures
are in each library's §4; this file points to them.

It follows [binding-contract.md](binding-contract.md). Rules are lettered
**R**.

## 1. Scope

FFmpeg has two families of side data, each with its own enumeration and its
own payload layouts:

| Family | C entry            | C kind                      | Bound in  |
| ------ | ------------------ | --------------------------- | --------- |
| packet | `AVPacketSideData` | `enum AVPacketSideDataType` | `avcodec` |
| frame  | `AVFrameSideData`  | `enum AVFrameSideDataType`  | `avutil`  |

A **carrier** is an object that holds a list of entries of one family:

| Carrier                              | Family | Meaning                                         | Read | Write                |
| ------------------------------------ | ------ | ----------------------------------------------- | ---- | -------------------- |
| packet (`AVPacket.side_data`)        | packet | applies to this packet                          | yes  | yes                  |
| codec parameters (`coded_side_data`) | packet | applies to the whole stream                     | yes  | by functional update |
| frame (`AVFrame.side_data`)          | frame  | applies to this frame                           | yes  | yes                  |
| encoder (`decoded_side_data`)        | frame  | stream-wide data of the frames it will be given | no   | at creation          |

Stream side data is the side data of the stream's codec parameters. It is
reached through `Av.get_codec_params` and needs no operation of its own.

Not bound, each for the reason given:

- side data on filter links, on the buffer source parameters and on the buffer
  sink: absent from the floor release (V4);
- side data of stream groups and tile grids: the bindings have no stream
  group;
- conversion of an entry between the two families: libavcodec does it inside
  decoders (§4) and encoders, which are the only places it is needed;
- the decoder option `side_data_prefer_packet`: decoders take no options.

### 1.1 Three levels

The interface has three levels. Each is reached from the one below by
translations that are themselves part of the interface, so that a program can
stop at any level and compose the rest itself.

| Level | Values                                                                                                               | Owner               |
| ----- | -------------------------------------------------------------------------------------------------------------------- | ------------------- |
| 1     | raw entries, as carriers hold them                                                                                   | `avutil`, `avcodec` |
| 2     | what an entry means: a display matrix, its rotation angle, the transformations it asks for, a cropping, a ReplayGain | `avutil`, `avcodec` |
| 3     | what to do about it: the filters to link, and a converter that runs them                                             | `avfilter`          |

| Translation | Functions                                                                                                    | Section                                                             |
| ----------- | ------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------- |
| carrier → 1 | `Frame.side_data`, `Frame.find_side_data`, `Packet.raw_side_data`, `Avcodec.params_side_data`                | [avutil.md](avutil.md) §4.20, [avcodec.md](avcodec.md) §4.2, §4.11  |
| 1 → 2       | `Frame_side_data.decode`, `.display_matrix`; `Packet_side_data.decode`, `.display_matrix`, `.cropping`       | [avutil.md](avutil.md) §4.20, [avcodec.md](avcodec.md) §4.11        |
| within 2    | `Display_matrix.rotation`, `Display_matrix.transforms`                                                       | [avutil.md](avutil.md) §4.19                                        |
| 2 → 3       | `Avfilter.Utils.filter_of_transform`, `Avfilter.Utils.filter_of_cropping`                                    | [avfilter.md](avfilter.md) §4.13                                    |
| 1 → 3       | `Avfilter.Utils.display_layout`, the display converter                                                       | [avfilter.md](avfilter.md) §4.13, §11.4, §11.5                      |
| 2 → 1       | `Frame_side_data.encode`, `Packet_side_data.encode`, `Display_matrix.make`                                   | [avutil.md](avutil.md) §4.19, §4.20, [avcodec.md](avcodec.md) §4.11 |
| 1 → carrier | `Frame.add_side_data`, `Packet.add_raw_side_data`, `Avcodec.params_with_side_data`, `?side_data` of encoders | the same, and [avformat.md](avformat.md) §4.4                       |

- **R0. Level 3 is a composition.** Where level 3 decides (R13), its
  filters are the composition of the functions above it, and a program that
  composes the translations by hand gets the same chain. What level 3 adds is
  the decision itself: `display_layout` is the one place that says whether a
  cropping and a display matrix have a single right answer.

- **R13. Level 3 decides nothing that has no single right answer.** Where
  the meaning of an entry leaves a choice (a rotation that is not a quarter
  turn, a cropping that leaves no picture, a frame no software filter can
  transform), the interface reports it as undecided and applies nothing. The
  ready-made converter asks its caller, and by default logs a warning and
  leaves the frame untouched, side data included
  ([avfilter.md](avfilter.md) §4.13).

## 2. Objects

### 2.1 Raw entry — `Packet_side_data.raw`, `Frame_side_data.raw`

```ocaml
type raw = private { kind : kind; data : string }
```

Plain OCaml data holding a copy of the payload (S1). It has no native object.

- **R1. A raw entry is produced only by the binding.** The record is private.
  A value of the type comes from reading a carrier or from `encode`. The
  interface has no constructor from bytes.

  FFmpeg components read a payload at the size its kind implies, with no
  bound check. A constructor from bytes would let a short payload reach them.

- **R2. A payload is meaningful only in the process that read it.** For kinds
  whose payload is a C structure the bytes have the layout of the FFmpeg build
  in use. A program MUST NOT store them or send them to another process.
  Display matrices, frame cropping and packed dictionaries have a layout
  FFmpeg documents; the typed interface owns it.

### 2.2 Carriers

No new handle. Packets, frames and codec parameters are the objects of
[avcodec.md](avcodec.md) §2 and [avutil.md](avutil.md) §2.

- **R3.** Codec parameters stay immutable. Changing their side data yields a
  new value.
- **R4.** Changing the side data of a packet or a frame is a change in the
  sense of M8: the caller is the only user at that moment.
- **R5.** A packet and a frame account for their payload only (L9). Side data
  is not added to the reported size.

## 3. Rules across libraries

- **R6.** A kind joins a typed table when an application has to act on its
  content. Every other kind is carried raw and passes through unchanged.
- **R7.** The kinds a program can name are those of the installed headers
  (G1). An entry whose kind has no constructor is left out of every list
  (E4).
- **R8. Parameters carry their side data everywhere.** Every operation that
  copies codec parameters copies their coded side data: `Av.get_codec_params`,
  `Av.new_stream_copy`, `Avcodec.params`, `BitstreamFilter.init`, and decoder
  creation from `?params`. A stream created by `Av.new_stream_copy ~params`
  has the side data of `params`.
- **R9.** The stream side data of an output is fixed when the stream is
  created. No operation changes it afterwards.
- **R10.** `?side_data` of `Av.new_audio_stream` and `Av.new_video_stream` is
  the argument of the same name of the encoder creation
  ([avcodec.md](avcodec.md) §4.11). What libavcodec copies to the coded side
  data reaches the stream.
- **R11.** Every payload is copied, in both directions (S1). No side-data
  operation shares memory with a native buffer, releases the runtime lock or
  takes a guard.
- **R12.** Nothing differs between the supported releases. Every function and
  constant used exists in the floor release.

## 4. What FFmpeg does on its own

Stated here because applications rely on it; the conformance suite checks the
first item.

- A decoder created from parameters attaches to every frame it outputs the
  stream side data whose kind libavcodec maps to a frame kind, the display
  matrix among them.
- A decoder may attach side data it finds in the bitstream, such as the
  display matrix of an image's EXIF orientation.
- A frame given to a filter graph keeps its side data, and the frames a
  filter outputs carry what the filter propagates.

## 5. Not verified

- That FFmpeg 7.1, 8.0 and 8.1 propagate the display matrix as §4 states.
  Read in the sources of 7.1; run against the development branch only.
- The conformance requirements of [tests.md](tests.md) §13 under the
  collection-at-every-allocation build and the sanitisers.
- Payloads of structure kinds on a big-endian host.
