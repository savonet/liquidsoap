# FFmpeg file and stream decoder

This file owns the FFmpeg decoder that Liquidsoap uses for files and byte
streams: which files and MIME types it accepts, how it opens and probes a
container, which content type a file decodes to, how container streams map to
frame fields, the decode loop, positions, seeking, remaining time, metadata,
duration resolution and end of stream. It defines the **per-stream decoder
contract** (§7) that the three per-stream decoder families implement, each in
its own file with the same numbering:
[decoder-internal.md](decoder-internal.md), [decoder-raw.md](decoder-raw.md),
[decoder-copy.md](decoder-copy.md). It also owns two helpers that several
producers share (§16): the internal video converter and the subtitle
timeline. The timed ID3 data-stream decoder is small and does not follow the
contract; it is specified in §10.3. The image decoder is in
[image-decoder.md](image-decoder.md).

The decoder relies on the FFmpeg bindings
([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md): opening,
`read_input`, `seek`), on the content types of [content.md](content.md), on
the display stage of [side-data.md §3](side-data.md#3-autorotate-on-decoding),
on the helper graphs of [filters.md §16](filters.md#16-helper-graphs), on the
units, conversion and duration helper of [common.md](common.md), on the
draining rule of [draining.md](draining.md), and on the fault model of
[faults.md](faults.md), which classifies every fault this file detects
(§13.2). Generic decoder and request
machinery is stated only where this decoder relies on it.

## 1. Registrations

The decoder registers five things at program load.

| Registry            | Name     | What it is                                                                         |
| ------------------- | -------- | ---------------------------------------------------------------------------------- |
| media decoders      | `ffmpeg` | file type probe, file decoder, stream decoder (§3, §5, §8)                         |
| duration resolvers  | `ffmpeg` | file duration (§11.1)                                                              |
| metadata resolvers  | `ffmpeg` | file tags (§10.4)                                                                  |
| image file decoders | `ffmpeg` | still image to one picture ([image-decoder.md](image-decoder.md))                  |
| (none)              | —        | the preferred-codec settings of §2, built from the list of FFmpeg decoders at load |

The media decoder entry is documented as "Use FFmpeg to decode any file or
stream if its MIME type or file extension is appropriate."

The generic request code turns the two resolvers into the script builtins
`request.duration.ffmpeg` and `file.metadata.ffmpeg`.

`request.duration.ffmpeg` has this type:

```
(?resolve_metadata : bool, ?metadata : [string * string], ?timeout : float?,
 string) -> float?
```

- `metadata` (default `[]`): "Optional metadata used to decode the file, e.g.
  `ffmpeg_options`." It is merged into the request's metadata and reaches the
  resolver of §11.1, which reads `ffmpeg_options` from it (§4).
- `resolve_metadata` (default `true`), `timeout` (default `null`).

`file.metadata.ffmpeg` has type `(string) -> [string * string]`. It calls the
resolver of §10.4 with no request metadata, lowercases every key, keeps the
first value of a repeated key (as the request path does), and returns `[]`
when the resolver fails.

## 2. Settings

| Key                                                    | Type        | Default                       | Meaning                                                                                                                   |
| ------------------------------------------------------ | ----------- | ----------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `settings.decoder.ffmpeg.max_interleave_duration`      | float       | `5.0`                         | longest span, in seconds, the decoder waits for all streams (§8.4); a value that is not positive is a configuration error |
| `settings.decoder.ffmpeg.codecs.<codec>`               | string      | unset                         | preferred FFmpeg decoder name for codec `<codec>` (§5.1)                                                                  |
| `settings.decoder.ffmpeg.codecs.<codec>.available`     | string list | FFmpeg decoders for `<codec>` | informative list of decoder names                                                                                         |
| `settings.decoder.mime_types.ffmpeg`                   | string list | 71 entries (§3.4)             | MIME types for media                                                                                                      |
| `settings.decoder.mime_types.ffmpeg.images`            | string list | 5 entries (§3.4)              | MIME types for images                                                                                                     |
| `settings.decoder.file_extensions.ffmpeg`              | string list | 290 entries (§3.4)            | extensions for media, images excluded                                                                                     |
| `settings.decoder.file_extensions.ffmpeg.images`       | string list | 58 entries (§3.4)             | extensions for images                                                                                                     |
| `settings.decoder.priorities.ffmpeg`                   | int         | `10`                          | priority of the media decoder and of the duration resolver                                                                |
| `settings.decoder.priorities.image.ffmpeg`             | int         | `10`                          | priority of the image decoder                                                                                             |
| `settings.request.metadata_decoders.priorities.ffmpeg` | int         | `1`                           | priority of the metadata resolver                                                                                         |

`<codec>` ranges over the codec identifier names (as FFmpeg prints them) of
every audio, video and subtitle decoder FFmpeg provides. The `available` list
holds the decoder names for that codec, the last registered first. Example:
`settings.decoder.ffmpeg.codecs.mp3.available()` is `["mp3", "mp3float"]`.

The decoder reads every list and priority at each use, so a change at runtime
takes effect for the next file.

## 3. Acceptance and priority

### 3.1 Files

The generic file-decoder selection offers a file to this decoder as follows:

1. The extension is the part after the last dot, lowercased. The MIME type is
   looked up from the file name.
2. The file passes when its extension is in
   `file_extensions.ffmpeg @ file_extensions.ffmpeg.images`, **or** the part of
   its MIME type before the first `;` equals the part before the first `;` of
   one entry of `mime_types.ffmpeg @ mime_types.ffmpeg.images`. A failed test
   logs at info level that the extension, or the MIME type, is unsupported.
3. Passing decoders are tried in decreasing priority (stable for equal
   priorities, in the order of `settings.decoder.decoders`). For each, the
   file type probe (§5) gives a content type; the first one whose content type
   can be converted to the requested one wins. "Can be converted" keeps only
   the requested fields, accepts an audio field whose channel layout can be
   converted, and then checks compatibility.

Every extension test of this file, the image decoder included, uses this
lowercased extension. Every MIME test compares the parts before the first
`;`. The default MIME list holds `application/octet-stream` twice; matching
treats the list as a set, so the duplicate has no effect.

With the default settings the decoders available for a file are listed as
`ffmpeg (priority: 10), image (priority: 1)`.

Conformance:

- `IMG.PNG` and `img.png` are accepted, rejected and optioned identically by
  every test of this file and of [image-decoder.md](image-decoder.md).

### 3.2 Images through the media decoder

When the lowercased extension is in `file_extensions.ffmpeg.images` and the
requested content type has a `video` field (the first video field only) of
internal video format, the probe returns the empty content type without
opening the file. The media decoder is then rejected and the generic `image`
decoder (priority 1) takes the file, which uses the image decoders of
[image-decoder.md](image-decoder.md).

An image requested as raw or copy video is opened by the media decoder with
the demuxer options `loop = 1` and `framerate = <video rate>` (§4.3). The
image demuxer loops forever with these options: the file decodes to an
endless video track. Example: a 64×48 PNG requested as
`source(video=ffmpeg.video.raw)` is still playing after 3 s.

### 3.3 Streams

A byte stream with MIME type `m` is offered to the stream decoders whose MIME
list contains an entry with the same part before `;` as `m`. The one with the
highest priority is used, with no probing. The FFmpeg stream decoder receives
the full MIME string (§4.2).

### 3.4 Default lists

`mime_types.ffmpeg` (71 entries, `application/octet-stream` twice):

```
application/f4v application/ffmpeg application/mp4 application/mxf
application/octet-stream application/octet-stream application/ogg
application/vnd.pg.format application/vnd.rn-realmedia application/vnd.smaf
application/x-mpegURL application/x-ogg application/x-pgs
application/x-shockwave-flash application/x-subrip application/xml audio/G722
audio/MP4A-LATM audio/MPA audio/aac audio/aacp audio/aiff audio/amr
audio/basic audio/bit audio/flac audio/g723 audio/iLBC audio/mp4 audio/mpeg
audio/ogg audio/vnd.wave audio/wav audio/wave audio/webm audio/x-ac3
audio/x-adpcm audio/x-caf audio/x-dca audio/x-eac3 audio/x-flac audio/x-gsm
audio/x-hx-aac-adts audio/x-ogg audio/x-oma audio/x-tta audio/x-voc
audio/x-wav audio/x-wavpack multipart/x-mixed-replace;boundary=ffserver
text/vtt text/x-ass text/x-jacosub text/x-microdvd video/3gpp video/3gpp2
video/MP2T video/mp2t video/mp4 video/mpeg video/ogg video/webm video/x-flv
video/x-h261 video/x-h263 video/x-m4v video/x-matroska video/x-mjpeg
video/x-ms-asf video/x-msvideo video/x-nut
```

`mime_types.ffmpeg.images` (5 entries):

```
image/gif image/jpeg image/png image/vnd.microsoft.icon image/webp
```

`file_extensions.ffmpeg` (290 entries):

```
264 265 302 3g2 3gp 669 722 A64 a64 aa aa3 aac aax ac3 acm adf adp ads adts
adx aea afc aif aifc aiff aix amf amr ams amv ape apl apm apng aptx aptxhd
aqt asf ass ast au aud avi avr avs avs2 bcstm bfstm binka bit bmv brstm c2
calf cavs cdata cdg cdxl cgi chk cif cpk cvg dat daud dav dbm dif digi dmf
dnxhd dnxhr drc dsm dss dtk dtm dts dtshd dv dvd eac3 f4v fap far ffmeta fits
flac flm flv fsb fwse g722 g723_1 g729 gdm genh gif gsm gxf h261 h263 h264
h265 hca hevc ice ico idf idx ifv imf imx ipu ircam ism isma ismv it ivf ivr
j2b jss kux latm lbc loas lrc lvf m15 m1v m2a m2t m2ts m2v m3u8 m4a m4b m4v
mac mca mcc mdl med mj2 mjpeg mjpg mk3d mka mks mkv mlp mmcmp mmf mms mo3 mod
mods moflex mov mp2 mp3 mp4 mpa mpc mpd mpeg mpg mpl2 mptm msbc msf mt2 mtaf
mtm mts musx mvi mxf mxg nist nsp nst nut obu oga ogg ogv okt oma omg opus
paf pcm pjs plm psm psp pt36 ptm pvf qcif ra rco rcv rgb rm roq rsd rso rt
s3m sami sbc sbg scc sdr2 sds sdx ser sf sfx sfx2 sga shn sln smi son sox
spdif sph spx srt ss2 ssa st26 stk stl stm stp str sub sup svag svs swf tak
tco thd ts tta ttml tun txt ty ty+ ult umx v v210 vag vb vc1 vc2 viv vob voc
vpk vqe vqf vql vtt w64 wav webm wma wmv wow wsd wtv wv xl xm xml xmv xpk
xvag y4m yop yuv
```

`file_extensions.ffmpeg.images` (58 entries):

```
bmp cri dds dng dpx exr im1 im24 im32 im8 j2c j2k jls jp2 jpc jpeg jpg jps
ljpg mng mpg1-img mpg2-img mpg4-img mpo pam pbm pcd pct pcx pfm pgm pgmyuv pic
pict pix png pnm pns ppm ptx ras raw rs sgi sun sunras svg svgz tga tif tiff
webp xbm xface xpm xwd y yuv10
```

`gif` and `ico` are in the media list, not the image list: they decode as
video through the media decoder.

## 4. Input options

### 4.1 `ffmpeg_options` metadata

Files take demuxer options from the request metadata `ffmpeg_options`. Its
value uses the argument syntax of the `%ffmpeg(...)` encoder: the string is
wrapped in parentheses and parsed as a list of encoder parameters. Each
parameter maps as follows.

| Parameter                                 | Result                                                               |
| ----------------------------------------- | -------------------------------------------------------------------- |
| `f=<identifier>` or `format=<identifier>` | forced input format: the demuxer named `<identifier>`                |
| `<key>=<identifier>`                      | option `<key>` with the identifier as a string                       |
| `<key>="<string>"`                        | string option                                                        |
| `<key>=<int>`                             | integer option                                                       |
| `<key>=<float>`                           | float option                                                         |
| anything else, or a parse error           | error of kind `ffmpeg_decoder` naming `ffmpeg_options` and the value |

A later parameter with the same key replaces the earlier one. Absent
metadata means no options and no forced format.

A forced format that names no FFmpeg demuxer is an error of kind
`ffmpeg_decoder` naming the format. The decoder MUST NOT fall back to
probing: a typo in `f=` would otherwise go unnoticed.

### 4.2 `application/ffmpeg` MIME parameters

A stream whose MIME type, split on `;`, starts with `application/ffmpeg` takes
the remaining parts, rejoined with `;`, as an option string parsed as in §4.1.
Any other MIME type gives no options.

### 4.3 Image options and unconsumed options

For a file whose lowercased extension (§3.1) is in
`file_extensions.ffmpeg.images`, and for a stream whose MIME type (part before
`;`) is in `mime_types.ffmpeg.images`, the options `loop = 1` and
`framerate = <video rate in frames per second>` are set, replacing any given
ones.

Opening a container consumes the options the demuxer recognises. Every
entry point that opens a container with these options checks that none
remain: the probe (§5), the file and stream decoders (§8), the duration
resolver (§11.1) and the metadata resolver (§10.4). Remaining options are an
error of kind `ffmpeg_decoder` that lists them, rendered as by the encoder
([encoder.md](encoder.md)). A file with a bad option thus fails at the probe,
with the error logged, rather than when it starts playing.

Conformance:

- `annotate:ffmpeg_options="foo=1,f=mp3":a.mp3` is rejected by the probe with
  an error naming `foo=1`; `request.duration.ffmpeg` on it does not use this
  resolver's value.
- `f=nosuchformat` is an error naming `nosuchformat`, at the probe and in
  both resolvers.

## 5. Opening and probing

### 5.1 Opening

The probe and the decoders open the container with the bindings'
`open_input` (files) or `open_input_stream` (streams), with the forced format
and the options of §4. The probe, the decoders and `input.ffmpeg` pass a
stream configuration per media type (audio, video, subtitle) that picks the
preferred decoder:

1. Take the codec identifier name of the stream's parameters.
2. When `settings.decoder.ffmpeg.codecs.<name>` is set, log at info level that
   the preferred decoder is tried, and look the decoder up by name.
3. An unset key gives the default decoder.

A key set to a name FFmpeg has no decoder for is a configuration error
([faults.md §2](faults.md#2-classes)), raised when the setting is set, so a
script that names an unavailable decoder fails before it streams. A decoder
that exists but cannot open a given stream is a source fault of that input
(§13.2).

The duration and metadata resolvers decode nothing and open without this
configuration.

The stream decoder passes a seek function when its input can seek. It
honours FFmpeg's `whence`:

| `whence`   | Byte position sought                                                                  |
| ---------- | ------------------------------------------------------------------------------------- |
| `SEEK_SET` | `offset`                                                                              |
| `SEEK_CUR` | the current byte position plus `offset`                                               |
| `SEEK_END` | the input's length plus `offset`; when the input has no known length, an error result |

The decoder keeps the current byte position itself: the result of the last
successful seek (0 at open) plus every byte read since. The function returns
the new byte position, or a negative result on failure.

### 5.2 Container description and content type

The probe opens the container (§5.1), describes it and derives the content
type it yields for the requested one, as
[stream-description.md §2](stream-description.md#2-container-format-url-input),
[§5](stream-description.md#5-describe) and [§6](stream-description.md#6-get_type-ctype-container)
specify: field numbering, entry formats, the specific format name, the
per-stream decoded formats, and the failure when no stream is usable. Between
the description and the content type, the decoder logs at important level
that FFmpeg recognises the URI as the description. The container is closed
afterwards, whatever the outcome.

Errors raised by the probe (open failure, remaining options, unification
failure) are logged by the generic selection at warning level, and the next
decoder is tried.

Example log of one probe (a Matroska file with AAC mono, H.264 and SRT,
requested as audio, video and subtitles):

```
[decoder.ffmpeg.description:3] Analyzing container for format: "matroska,webm", uri: "/…/av.mkv"
[decoder.ffmpeg:3] FFmpeg recognizes "/…/av.mkv" as audio: {codec: aac, 44100Hz, 1 channel(s)}, video: {codec: h264, 320x240, yuv444p}, subtitles: {codec: subrip, subtitle}
[decoder.ffmpeg.description:3] Requested content-type: {audio=pcm(stereo),subtitles=subtitles,video=yuv420p}
[decoder.ffmpeg.description:3] Decoded content-type: {audio=pcm(mono),subtitles=subtitles,video=yuv420p(alpha=false)}
```

The mono decoded audio is accepted for the stereo request because the
channel layout converts (§3.1).

## 6. Stream selection and field mapping

When a decoder is created, each container stream is mapped to at most one
per-stream decoder. The mapping uses the stream description of
[stream-description.md §3](stream-description.md#3-fields): the same
enumeration of streams, the same skipping of a stream whose parameters cannot
be read (logged as a warning there), and the same field names (`audio`,
`audio_2`, …). The probe and the decoder MUST derive their fields from one
description of the container, so the content type the probe returns and the
fields the decoder fills name the same streams. A stream is selected when the
requested content type has its field; the requested format picks the
per-stream decoder.

| Stream        | Requested format           | Read as | Per-stream decoder                                                                         | Sparse |
| ------------- | -------------------------- | ------- | ------------------------------------------------------------------------------------------ | ------ |
| audio         | copy                       | packets | copy audio ([decoder-copy.md](decoder-copy.md))                                            | no     |
| audio         | raw audio                  | frames  | raw audio ([decoder-raw.md](decoder-raw.md))                                               | no     |
| audio         | PCM (float, s16, f32)      | frames  | internal audio, channel count from the format ([decoder-internal.md](decoder-internal.md)) | no     |
| video         | copy                       | packets | copy video                                                                                 | no     |
| video         | raw video                  | frames  | raw video                                                                                  | no     |
| video         | internal video             | frames  | internal video                                                                             | no     |
| subtitle      | copy                       | packets | copy subtitles                                                                             | yes    |
| subtitle      | text subtitles             | frames  | internal text subtitles                                                                    | yes    |
| subtitle      | internal video             | frames  | internal bitmap subtitles                                                                  | yes    |
| data          | — (codec `timed_id3` only) | packets | timed ID3 (§10.3)                                                                          | yes    |
| anything else |                            |         | not read                                                                                   |        |

For internal video, before the decoder is built, the stream's expected
display size ([side-data.md §3](side-data.md#3-autorotate-on-decoding)) is
offered to the frame size negotiation as the ideal size with source name
`ffmpeg decoder`; the decoder then uses the width and height of the requested
format, and the format's alpha is set from the stream's pixel format when not
yet decided.

Every per-stream decoder of one mapping shares one stream index
([content.md §5](content.md#5-stream_idx-and-time_base)), carried by copy and
raw content. A new stream index is taken when a mapping is built (at open and
at every seek, §12) and at every timestamp discontinuity (§9.2). Each is a
source switch downstream.

Conformance:

- For a file whose first audio stream has unreadable parameters, the probed
  content type and the decoded fields name the same streams.

## 7. Per-stream decoder contract

Each per-stream decoder implements this contract. The implementation files
answer every clause, with the same numbers, §1 to §8.

### 7.1 Selection

The row of the table in §6 that creates the decoder, and what it needs at
creation: the stream, its codec parameters, the field, the requested format,
and for copy and raw the stream index.

### 7.2 Construction

What the decoder computes once at creation, and what it raises when the
stream does not fit the requested format. A raise aborts the decoder
creation and the file is not played.

### 7.3 Input

A decoder receives, one call at a time:

- items of one kind (decoded frames, packets, or subtitle frames), in the
  order the decode loop releases them (§8.4), which for one stream is
  demuxing order;
- `switch i`: from now on the content carries the stream index `i`, marked
  continuing or not (§9.2);
- for sparse decoders, `advance p` (§7.7);
- a final `Flush` at end of input (§13.1).

Items before the start of the mapping or a seek target never reach the
decoder (§8.5).

### 7.4 Output

What the decoder writes to the output: content chunks put on its field,
metadata added at the current position. Output position is implicit: each
field grows by the length of what is put, and metadata is added at the
current length of the output.

### 7.5 Timestamps

Which timestamps the decoder reads, their time base, how lengths are derived,
and which timestamps it leaves in the content. Lengths derived from
timestamps re-anchor to absolute values ([common.md §6](common.md#6-duration-helper)).
Decreasing timestamps never reach a decoder (§9.2).

### 7.6 Flush

`Flush` drains the decoder ([draining.md](draining.md)): every stage it holds
is told that no more input follows, and everything they then produce is put
on the field, in order, as ordinary output. After `Flush` the decoder holds
nothing. `switch` drains in the same way the data that belongs to the
previous stream index, when the decoder holds any.

### 7.7 Sparse advance

Sparse decoders (subtitles, timed ID3) receive, besides items, `advance p`
with `p` the position, in main ticks, reached by every non-sparse stream
(§8.3). It lets them fill their field up to `p` when no item arrives.

### 7.8 Errors and logs

What the decoder drops and why, and what it logs, each fault with its class
([faults.md §2](faults.md#2-classes)). A dropped item is either specified
with its reason or an item fault logged as a warning and counted toward the
loop's bound (§8.2). A per-stream decoder never raises out of an item call:
it reports an item fault (the item is dropped) or a source fault (the loop
ends the track) to the decode loop. Faults at construction are source faults
of the input.

## 8. Decode loop

### 8.1 State

Per decoder record (one per opened container, §15):

- the stream map: container stream index → per-stream entry (§6) with its
  time base, sparse flag, and the per-stream positions of §8.3; rebuilt on
  seek;
- the container origin `O` and the mapping start `B` (§8.3);
- the target: none, or a seek target in main ticks (§12), per stream until
  that stream reaches it (§8.5);
- the pending queue (§8.4);
- whether a discontinuity occurred since the mapping was built (§9.2);
- the last container metadata and the last metadata of each stream (§10).

Main ticks are those of [common.md §4](common.md#4-liquidsoap-time-bases).

### 8.2 One decode step

The caller (the generic file decoder, `input.ffmpeg`, the harbor) calls
`decode` repeatedly until its output holds enough data. One call:

1. Reads the container metadata; emits it as §10.1 states.
2. Reads one item with the bindings' `read_input`, passing the selected
   streams in their read mode
   ([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md), `read_input`).
   An item of a stream not in the map is discarded and the call reads again.
3. Takes the item's timestamps (§9) and checks them (§9.2).
4. For an audio or video stream, reads the stream metadata (§10.2).
5. Before the mapping has started, queues the item (§8.4). After it has
   started, applies the target (§8.5), updates the positions (§8.3) and gives
   the item to its per-stream decoder.
6. Errors of `read_input`:
   - `Eagain` (a custom read that would block): the call reads again. The
     input's read function owns the wait; a read that never returns data is
     ended by the input's interrupt, which gives `Exit`.
   - `Invalid_data`: an item fault; logged as a warning and the call reads
     again, within the bound below.
   - `Eof`: end of input (§13.1).
   - `Exit` (a blocking call aborted by the input's interrupt): an abort,
     reported to the caller distinctly from end of input. The caller decides
     between draining (`eof`) and discarding (`close`).
   - any other error: a source fault; the input ends with that error
     (§13.2).

`decode` never raises while the input streams: every outcome above is a
result the caller reads (data, end of input, abort, or a source fault with
its error). The caller applies [faults.md §3](faults.md#3-source-faults-reach-the-script).

`max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate))
counts every item the decoder rejects (an `Invalid_data` read, a decreasing
timestamp of §9.2, an item a per-stream stage fails on, §13.2) and is reset
by every item released successfully. It bounds the time a decode step can
spend on an input that never yields a valid item, which would otherwise hold
the caller forever, and it is what tells a glitch (dropped, decoding goes on)
from media that cannot be decoded any further (the input ends). Past it, the item faults become a source fault. The first drop of each kind
is logged as a warning; repeats are counted and logged with their count at
most once per `fault_log_interval`
([faults.md §4](faults.md#4-item-faults-escalate)). Faults whose item carries
no media (a metadata entry, a seek) are logged the same way and not counted.

### 8.3 Positions and sparse advance

This section is the single owner of content positions on the decoding side.
All timestamps are converted to main ticks with the layer's `rescale`
([common.md §5](common.md#5-time-base-conversion)), from the time base they
are expressed in (§9). An item's **key** is its DTS for a packet and its
timestamp of §9 for a frame (PTS, else best-effort): copied packets are
positioned by DTS, the timestamp that increases in decode order, and decoded
frames by their presentation timestamp. Its **end** is its PTS
plus its duration (the PTS alone when it has no duration).

- `O`, the **container origin**: the input's start time as FFmpeg reports it,
  in main ticks; 0 when FFmpeg reports none. Positions the caller sees
  (current position, seek target, remaining time) count from `O`.
- `B`, the **mapping origin**: the smallest key among the first items the
  non-sparse streams release (§8.5); for an all-copy mapping, the smallest
  first released DTS. Position 0 of the copy and sparse
  fields stands for it. §8.4 says when it is fixed.
- Per stream `s`, an origin `o_s`:
  - copy streams and sparse streams: `o_s = B`;
  - decoded streams (internal and raw): `o_s = B` as well. A decoded field
    cannot start with a gap without inventing content
    ([faults.md §3](faults.md#3-source-faults-reach-the-script)), so §8.5
    starts every stream of a mapping that holds a decoded stream at one start
    point: each decoded stream's first item contains it, and its position 0
    is that item, to within one item of `B`.
  - A discontinuity moves `o_s` (§9.2).
- The **content position** of an item of stream `s` is
  `rescale(key) − o_s`. Every producer of the mapping places content at this
  position: the duration helper of a copy or raw decoder is created with
  `start = rescale(first key) − o_s` (§9.1); a copy field whose first item is
  after position 0 starts with an empty run of that length
  ([content.md §4](content.md#4-two-chunk-layers)); a sparse item stands at
  `rescale(PTS) − o_s`.
- Per non-sparse stream, the progress `p_s = rescale(end) − o_s` of its
  latest released item. The decoder position `P` is the minimum `p_s` over
  the non-sparse streams that have released an item; before any release, 0.
- When `P` grows, every sparse stream receives `advance P` (§7.7).

The position the caller sees is `U = (B − O) + P` once the mapping has
started, to within one item. Before that, `U` is the target minus `O` when a
target is set, else 0.

A packet with no timestamp keeps its place in its stream's order and changes
no position. A decoded frame with no timestamp follows §9.

Conformance:

- On an MPEG-TS file whose start time is 1.4 s, `U` is 0 at the first item
  and the subtitles of the same file land at the same positions as on a
  start time of 0.
- A copied H.264 stream whose first DTS is −3528 ticks and a copied AAC
  stream whose first DTS is −1024: the audio field starts with an empty run
  of 2504 ticks ([timestamps.md §11](timestamps.md#11-worked-examples)).

### 8.4 Waiting for all streams

A mapping starts only once every selected non-sparse stream has produced a
timestamped item. A live input can carry a long stretch of audio before the
first video frame; starting at once would put that audio against later video
and desynchronise the start.

Until the start, items join the pending queue. Each stream keeps its own
items in demuxing order. The mapping starts at the first of:

1. every selected non-sparse stream has produced a timestamped item;
2. the queue spans more than `max_interleave_duration`, measured on the
   release keys below;
3. end of input (§13.1);
4. a discontinuity (§9.2).

The **start threshold** `T0` (§8.5) is the target when one is set and later
than the following value, else that value:

- case 1: the latest first timestamp among the decoded non-sparse streams;
  none when every non-sparse stream is copied, since copied fields carry a
  leading gap as an empty run;
- cases 3 and 4: the earliest first timestamp among the decoded non-sparse
  streams seen.

Case 2 does not start the mapping. A selected stream can start late (a live
input whose video comes after its audio), so the decoder keeps waiting with a
**sliding window**: while the queue spans more than
`max_interleave_duration`, it drops the oldest queued items. This is an
intended discard, not an item fault: it is logged once as a warning naming the
streams not seen, and the dropped items are counted in that log at the
interval of [faults.md §4](faults.md#4-item-faults-escalate). Meanwhile
`decode` returns no data, so a live source is not ready until the stream
appears; the mapping then starts as case 1. End of input (case 3) releases
what the window holds: a file whose stream never appears plays the other
streams from the window.

When no non-sparse stream is selected, the mapping starts at once and `B` is
the target when one is set, else `O`.

On start, the queue is released: items go through §8.5 as a merge of the
per-stream queues, each stream in its own order, and across streams the item
with the smallest **release key** first. The release key is the item's key
(§8.3) converted to main ticks, so items of streams with different time
bases compare correctly. A packet with no timestamp takes the key of the item
before it in its stream. Ties keep demuxing order. `B` is fixed once every
non-sparse stream has released its first item.

`max_interleave_duration` bounds the data held while waiting, and so the
memory: frames read as decoded frames are held decoded. 5 s, the default, is
far above the stagger of streams in valid media.

Conformance:

- A live input whose audio starts 2 s before its video, both decoded: the
  first decoded audio and video positions match to within one audio frame.
- A live input whose video appears 8 s after its audio: no data for 8 s,
  one warning, then both streams start together; the audio of the first 3 s
  is counted as dropped.
- A file with a selected stream that never yields an item: the last
  `max_interleave_duration` of the other streams plays at end of input.
- A file shorter than `max_interleave_duration` with such a stream: every
  item reaches the output at end of input.

### 8.5 Start and seek threshold

The threshold of every stream is the **start point** when the mapping
starts:

- no `T0` (§8.4; every non-sparse stream copied): none, and every item is
  released;
- a mapping with decoded streams and a copied stream whose codec needs key
  packets (video that is not intra-only): the last key packet of that
  copied stream whose PTS is at or before `T0` when `T0` minus its PTS is at
  most `start_key_tolerance`, else the first key packet whose PTS is at or
  after `T0`. Every stream starts there:
  the copied video from that key packet, decoded and other copied streams
  from their first item ending at or after its DTS, the items before
  trimmed. All share one origin, `B` = that DTS, so positions agree: copied
  packets are positioned by DTS (§8.3), and the encoder presents the key
  picture its reorder delay after it, as it does for every copied group. The cost is up to one
  key-packet interval trimmed at the start of a file or after a seek; the
  reason is A/V sync without invented content. With several such copied
  streams, the start point is the latest of their first key packets at or
  after `T0`. Until the start point is known the items stay queued (§8.4),
  within `max_interleave_duration`. `start_key_tolerance` is a parameter,
  recommended value one audio frame (1024 samples at the audio rate): a key
  packet that close before `T0` costs less than a frame of decoded data
  before the threshold, where the next one may cost a whole key-packet
  interval;
- otherwise: `T0`.

Against a threshold `T`:

- a copied stream whose codec needs key packets (video) releases from its
  last key packet at or before `T` (the key packet at `T` itself when `T` is
  a start point of the mixed case): the packets before it cannot be decoded
  by any player, and the packets after it are needed to decode the first
  picture at or after `T`. While below the threshold, the decoder keeps the
  packets of such a stream from its latest key packet on, and replaces them
  at each new key packet;
- any other item is released when its end is at or after `T` (inclusive: a
  packet that ends exactly at `T` is kept).

Once a stream has released an item, its threshold is cleared: later items of
the stream are always released, since its timestamps only increase (§9.2).

Items below the threshold are read and not released. Frames read as decoded
frames have gone through FFmpeg's decoder all the same. This is what brings
codecs that carry state across packets (an mp3 bit reservoir) back to speed
after a seek: without it, up to about 150 ms of silence follows the seek point.
The number of items skipped per stream is logged at debug level. These items
are an intended discard, not item faults.

A packet with no timestamp, below the threshold, follows the decision taken
for the item before it in its stream; at the head of a stream, it is not
released.

The accuracy of a start or a seek is one item for every stream in a mapping
without copied video, and in an all-copy mapping for every stream but the
copied video, which starts up to one key-packet interval early. In a mixed
mapping every stream starts at the start point, up to one key-packet
interval after `T0`.

Conformance:

- Seeking to 2 s in a 4 s, 32 kbps mono mp3, the RMS over the 0.1 s after the
  seek exceeds 0.1.
- Two 3 s Vorbis files chained in one Ogg file, played with
  `liq_cue_in=1.0`: about 5 s of audio.
- A copied H.264 stream with `liq_cue_in=1.0` starts on a key packet at or
  before 1 s.
- An all-copy file whose streams start at different timestamps keeps every
  packet ([§13.1](#131-end-of-input) conformance).
- A file decoded to PCM audio with copied H.264 video, with
  `liq_cue_in=1.0`: both fields start at the first key packet at or after
  1 s, and their positions agree to within one audio frame.

### 8.6 Exclusion

Decode steps, `seek`, `eof` and `close` of one decoder record exclude each
other: each runs as if alone. A decode step that starts after `seek` returns
reads with the new mapping and the new target; a decode step that started
before `seek` completes with the old mapping and the old target. `close`
during a decode step takes effect when the step ends.

## 9. Timestamps on the decoding side

| Timestamp                | Time base                                | Used for                                                                           |
| ------------------------ | ---------------------------------------- | ---------------------------------------------------------------------------------- |
| packet DTS               | stream                                   | key: release order, positions, monotonicity (§8.3, §8.4, §9.2); copy chunk lengths |
| packet PTS               | stream                                   | item end, thresholds (§8.5); copy subtitle positions                               |
| frame timestamp          | stream                                   | key; raw lengths; internal video frame rate                                        |
| subtitle frame PTS       | FFmpeg's internal time base, `1/1000000` | key and sparse positions, converted from `1/1000000`                               |
| subtitle display times   | milliseconds, relative to the PTS        | internal subtitle start and end                                                    |
| packet or frame duration | stream                                   | item end (§8.3, §8.5); copy flush length                                           |

The **frame timestamp** is the frame's PTS, or else its best-effort
timestamp. This section owns that rule; the per-stream files link it. A
decoded audio frame with neither takes the key of the item before it in its
stream, since decoded audio is continuous; it is an item fault (dropped with a
warning, counted toward `max_consecutive_faults`) only when it is the first
of its stream. A decoded video frame with neither is an item fault.

Every conversion uses the time base the value is expressed in, with the
layer's `rescale` ([common.md §5](common.md#5-time-base-conversion)): rounded
to nearest, and a result that overflows is an invalid timestamp, an item
fault. Positions are differences of converted absolute timestamps
([common.md §6.0](common.md#60-absolute-origin)), never sums of converted
steps.

### 9.1 Grouping by timestamp increments

Copy and raw decoders cut their items into content chunks with the layer's
duration helper ([common.md §6](common.md#6-duration-helper)): DTS mode for
packets, PTS mode for frames, the source time base the items' own, offset 0,
`convert_ts` off so items keep their timestamps in that time base, and
`start = rescale(first key) − o_s`, so the helper's positions are the
content positions of §8.3. Consequences for the decoders:

- a chunk is emitted when an item at a later position arrives; it holds the
  previously held group and is as long as that step, so content lags the
  timestamps by one group until `Flush` or `switch` drains the helper
  ([common.md §6.5](common.md#65-draining-and-discarding));
- items at equal positions or with no timestamp travel in the held group;
  equal timestamps are valid input;
- chunk lengths are differences of absolute converted timestamps
  ([common.md §6.0](common.md#60-absolute-origin)), so the total length of a
  stream's content equals its timestamp span to within one tick, whatever its
  length; the conformance items are
  [common.md §6.6](common.md#66-conformance)'s;
- at `switch`, the decoder flushes its helper and creates a new one with
  `start = rescale(first key after the switch) − o_s`, with the moved origin
  of §9.2; for the stream that triggered the discontinuity this is the old
  helper's end position ([common.md §6.4](common.md#64-last_ts-t));
- at a seek the helper is dropped with its decoder, without flushing.

The helper's results follow [common.md §6.2](common.md#62-push-t-item): a
single decreasing timestamp is "dropped", an item fault; a run past
`max_consecutive_faults`, or a group past `max_group_items`, is
"discontinuity", a source fault: the decoder flushes the helper and ends the
track (§13.2). The decode loop's check (§9.2) runs first, so the helper
normally sees no decreasing timestamp.

### 9.2 Discontinuities and invalid timestamps

The decode loop checks, per stream, that the key never decreases. Equal
values are valid: files exist with two successive packets of the same DTS.

A decrease is a **discontinuity** when the input format declares timestamp
discontinuities legitimate (FFmpeg's `AVFMT_TS_DISCONT` flag, for example
MPEG-TS) or when the input is Ogg, where a chained stream restarts its
timestamps at each link. A discontinuity is one event for the whole mapping:

1. At the first stream `s` whose key decreases, at an item with key `t`, the
   decoder fixes the shift `Δ = rescale(t) − e_s`, where `e_s` is the content
   position at which the stream's content ends. It takes one new stream
   index.
2. Stream `s` **crosses**: `o_s` becomes `o_s + Δ'`, with `Δ'` chosen so its
   position continues (`rescale(t) − o_s = e_s`), and `s` receives `switch`
   with the new index. Its item is then processed normally.
3. Every other stream crosses at its own decrease, with `o_r = o_r + Δ'`
   (the same shift), and receives `switch` then. Until it crosses, its items
   stay on the old timeline under the old index. A stream never carries the
   new index on old-timeline items, nor the old index on new-timeline items.
4. Sparse streams cross with the first stream, since no item of theirs is in
   flight; a later sparse item behind the reached position is handled by the
   subtitle timeline as a late item (§16.2).
5. A decrease in a stream that has already crossed, before every non-sparse
   stream has crossed, is not a new event: it is an item fault.

The same shift for all streams keeps their relative timing across the jump.

The new stream index of a discontinuity is marked **continuing** when the
bitstream itself goes on: the input format declares discontinuities
(`AVFMT_TS_DISCONT`) and no stream's codec parameters change at the jump.
A copied stream under a continuing index stays decodable without a new key
packet, so the encoder rebases it without waiting for one
([encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state)).
An Ogg link starts a new bitstream with its own headers: its index is not
continuing.

Any other decrease is an item fault. The decoder drops the item with a
warning naming the stream and both timestamps, and decoding continues; the
stream's last key stays the one before the dropped item. When
`max_consecutive_faults` items in a row are rejected (§8.2), the timeline has
restarted for good: a source fault, and the track ends (§13.2). The decoder
does not reorder or rewrite timestamps.

Conformance:

- A chained Ogg file (two 3 s Vorbis links) plays about 6 s, with one
  `switch` per stream at the join for copy and raw requests.
- An MPEG-TS input whose audio and video jump back a few packets apart
  continues with one new stream index, each stream switching at its own
  jump.
- A Matroska file with one packet whose DTS goes back: that packet is
  dropped with a warning and the file plays to its end.
- A Matroska file whose timestamps restart for good ends after
  `max_consecutive_faults` dropped items, with an error; the content before
  it is drained.

## 10. Metadata

`decode_first_metadata` is false for files and true for streams,
`input.ffmpeg` and the harbor (§15). For a file, the request already carries
the file's tags (§10.4), so the first list of each metadata source is the
baseline and only later changes are news. For a stream there is no request,
and the first list is news.

### 10.1 Container metadata during decoding

At every decode step (§8.2, step 1) the container metadata (all key/value
pairs FFmpeg holds for the container) is read. A non-empty list that differs
from the last list of the container is a change. The decoder adds a change to
the output at its current position. When `decode_first_metadata` is false,
the first list is added only when it holds a key, or a value for a key, that
the request's metadata lacks; the file decoder is given the request's
metadata for this comparison. The request's tags come from the resolvers,
which may not be this decoder's (§10.4), so the file can carry tags they did
not return.

### 10.2 Stream metadata during decoding

Before each item of an audio or video stream, that stream's metadata is read
and then cleared in FFmpeg. Clearing avoids memory growth in FFmpeg with
chained Ogg streams, which append each link's tags. A non-empty list that
differs from the last list of that stream is a change, emitted under the rule
of §10.1.

Decoded audio and video frames may carry metadata of their own; the internal
decoders add it ([decoder-internal.md §4](decoder-internal.md#4-output)).

Conformance:

- A chained Ogg file whose second link has `title=two`: the title change
  reaches the output at the join, once.
- For a file, a tag list identical to the request's file tags is not
  emitted again at position 0.

### 10.3 Timed ID3

A data stream whose codec is `timed_id3` gets a decoder that, for each
packet, parses the payload as ID3v2 and adds the resulting tags to the output
when not empty:

1. A payload shorter than 3 bytes gives no tags.
2. A payload starting with `ID3` is parsed as is.
3. Otherwise the payload is prefixed with the 5 bytes `I D 3 0x03 0x00` and
   parsed; when that fails, with `I D 3 0x04 0x00`.
4. A payload that does not parse is an item fault: logged as a warning,
   no tags. It does not count toward `max_consecutive_faults`: the media
   around it decodes.

Some FFmpeg demuxers strip the ID3 header from timed ID3 payloads (FFmpeg
ticket 9540); step 3 rebuilds it. The decoder holds nothing: `Flush`,
`switch` and `advance` do nothing.

### 10.4 Tags for requests (metadata resolver)

Called with the request metadata, the lowercased extension (or none), the
MIME type and the file name:

1. The file must pass the test of §3.1 against the **media** lists only
   (`file_extensions.ffmpeg`, `mime_types.ffmpeg`); otherwise the result is
   empty.
2. The file is opened with the options of §4.1, without the stream
   configuration, and remaining options are checked (§4.3).
3. The result is the tags of FFmpeg's best audio stream (none when there is
   no audio stream), followed by the container tags, each key `track`
   renamed to `tracknumber` (exact, case-sensitive match).
4. A tag whose lowercased key is `duration` carries the request layer's
   duration, in seconds. A value of the form `HH:MM:SS.fraction` (Matroska's
   per-stream `DURATION` tag) is converted to a decimal number of seconds. Any
   other value that is not a decimal number is left out, with a warning.
5. The container is closed.
6. Any error other than the extension test is logged at warning level, and the
   resolver reports no tags.

The generic request code lowercases keys, recodes values to UTF-8, and keeps
the first value of a repeated key. With its priority of 1 this resolver is
below the request metadata (priority 5): request metadata wins over its tags.

Examples, from `file.metadata.ffmpeg`:

```
a.mp3  → [("artist", "Me"), ("encoder", "Lavf63.7.100"), ("title", "Hello"), ("tracknumber", "3/10")]
av.mkv → [("duration", "3.023"), ("encoder", "Lavc… aac")]
```

Conformance:

- `request.duration.ffmpeg("av.mkv")` is `3.023` with and without
  `resolve_metadata`.

## 11. Duration and remaining time

### 11.1 Duration resolver

Registered with the decoder priority (10) and the **media** extension list
only (no MIME check, no image extensions). Given the request metadata and a
file:

1. Opens the file with the options of §4.1 (no stream configuration) and
   checks remaining options (§4.3).
2. Reads the container duration in milliseconds, rounded by FFmpeg to the
   nearest unit, and divides by 1000.
3. Closes the container. No duration means no result, so the next resolver
   is tried.

Examples: `request.duration.ffmpeg("a.mp3")` is `3.0`; for `av.mkv` it is
`3.023`; a PNG gives `null`.

The generic request code checks the `duration`, `cue_in` and `cue_out`
metadata before calling resolvers.

### 11.2 Remaining time

The decoder record reports the remaining time from the container duration
`D` (in ticks, as §11.1; unknown when FFmpeg reports none or reading it
fails):

- `remaining = max(0, D − U)`, with `U` the position of §8.3;
- −1 when `D` is unknown, which the request layer reads as "unknown".

Both `D` and `U` count from the container origin, so the remaining time
reaches 0 at the end of the file whatever its start time. The generic file
decoder adds the length of its buffered output to this value.

Conformance:

- An MPEG-TS file with start time 1.4 s and duration 3.004 s reports a
  remaining time of about 2.98 s after 0.02 s of playback, and never a
  negative one.

## 12. Seeking

`seek x` moves `x` ticks forward (backward when negative) and returns the
number of ticks it moved.

1. When a discontinuity occurred in the current mapping (§9.2), positions no
   longer match container timestamps: `seek` logs a warning and returns 0.
2. The current position is `U` (§8.3). A target set by an earlier seek
   counts as the position until an item is released, so two seeks in a row
   add up.
3. The target, relative to the origin, is `u = max(0, U + x)`; in absolute
   ticks, `T = O + u`. The decoder logs the target in seconds at important
   level.
4. The decoder seeks the container with the bindings' `seek` to
   `ts = max_ts = T − seek_preroll` (0 when negative) with `min_ts` unbounded,
   so FFmpeg lands on a seekable point at or before that time.
5. When the container seek fails (an item fault: one request not honoured,
   not counted toward the bound), the decoder logs a warning with the error,
   keeps its mapping, position and target unchanged, and returns 0. FFmpeg
   leaves the input where it was on failure.
6. On success the decoder discards what the current mapping holds, without
   draining it ([draining.md §5](draining.md#5-discarded-on-purpose)): the
   per-stream decoders and the pending queue. It builds a new mapping (§6)
   with the same `decode_first_metadata`, a new stream index and fresh
   positions, and sets the target `T`. Downstream sees a source switch.
7. Returns `u − U`.

`seek_preroll` is a parameter, recommended value 0.5 s: the stretch decoded
before the target so that codecs with inter-packet state produce correct
output from the target on (§8.5).

Decoding resumes at the landing point. The new mapping starts at `T` or
later (§8.4), so the first content is the item containing the target.

The generic file decoder seeks within its buffered output when
`0 ≤ x ≤ buffered`; otherwise it empties the buffer and calls
`seek (x − buffered)`, reporting `buffered + result`.

Conformance:

- With `liq_cue_in=2.0` on a 3.004 s MPEG-TS file whose start time is 1.4 s,
  about 1.0 s plays.
- Two seeks of 1 s each before any decode step land at 2 s, not past the end.
- A failing container seek returns 0, and decoding continues as before.

## 13. End of stream and errors

### 13.1 End of input

`read_input` raising `Eof` is end of input; `Exit` is an abort (§8.2) and
reaches the caller as such. The bindings have
already drained FFmpeg's frame decoders: every frame they held has been
returned. The decoder signals end of input to its caller as end of input,
distinct from an error; a normal end of file logs at info level at most.

`eof` drains the decoder ([draining.md](draining.md)), upstream first:

1. When the mapping has not started, it starts (§8.4, case 3) and the pending
   queue is released.
2. Every per-stream decoder of the mapping receives `Flush`. Timed ID3 holds
   nothing.
3. The decoder adds a track mark.

What `Flush` puts goes into the generic file decoder's buffer like any other
content. This layer requires of that buffer, and of the generic frame code
behind it, that every field's content put before the track mark is
delivered. When fields have different lengths at that point (a copied audio
stream that ends 60 ms after its video, for instance), the track ends at
the end of the longest field and the longest is never cut to the shortest.
The tail of a shorter field carries no data: a gap, never invented content
([faults.md §3](faults.md#3-source-faults-reach-the-script)). Example:
in [timestamps.md §11.1](timestamps.md#111-container-start),
the final audio packet ends after the video and MUST reach the output.

Conformance:

- [draining.md §6](draining.md#6-conformance), for each per-stream decoder
  family: copy, raw and internal paths of one file give the same number of
  packets, frames or samples as the file holds, the last copied packet
  included, with and without a video field. For an all-copy mapping the
  count is the whole file's; for a mapping that mixes decoded streams with
  copied video, the count is from the start point (§8.5).

### 13.2 Errors

Every fault is placed in a class of [faults.md §2](faults.md#2-classes).

| Where                   | Fault                                                                                       | Class                | Effect                                                                                 |
| ----------------------- | ------------------------------------------------------------------------------------------- | -------------------- | -------------------------------------------------------------------------------------- |
| settings (§2, §5.1)     | unknown preferred decoder, non-positive interleave span                                     | configuration error  | error when the setting is set; the script does not start                               |
| option parsing (§4)     | invalid `ffmpeg_options`, unknown forced format                                             | source fault         | the request is rejected by the probe and both resolvers; it never plays                |
| opening (§4.3)          | remaining options                                                                           | source fault         | as above                                                                               |
| probe                   | no usable stream, open failure                                                              | source fault         | this decoder is rejected for the file; the next decoder is tried                       |
| decoder creation        | open failure, a stream unfit for its requested format                                       | source fault         | the track does not start                                                               |
| `read_input`            | `Eagain`                                                                                    | —                    | read again; the input's interrupt bounds the wait ([io.md](io.md))                     |
| `read_input`            | `Invalid_data`                                                                              | item fault           | warning, read again; counted (§8.2)                                                    |
| `read_input`            | `Eof`                                                                                       | —                    | end of input (§13.1)                                                                   |
| `read_input`            | `Exit`                                                                                      | —                    | abort reported to the caller (§8.2)                                                    |
| `read_input`            | any other error                                                                             | source fault         | end of track with the error                                                            |
| decode loop             | invalid decreasing timestamp (§9.2)                                                         | item fault           | item dropped with a warning; counted                                                   |
| decode loop             | `max_consecutive_faults` item faults in a row (§8.2)                                        | source fault         | end of track with the error                                                            |
| decode loop             | `max_interleave_duration` reached while waiting (§8.4)                                      | — (intended discard) | oldest queued items dropped, warned once and counted; no data until the stream appears |
| per-stream decoder      | a stage fails on one item; a corrupt packet                                                 | item fault           | item dropped with a warning; counted                                                   |
| per-stream decoder      | duration helper "discontinuity" (§9.1)                                                      | source fault         | helper flushed; end of track with the error                                            |
| per-stream decoder      | a video frame, or the first audio frame, with no timestamp; a timestamp that overflows (§9) | item fault           | item dropped with a warning; counted                                                   |
| metadata (§10.3, §10.4) | unparsable timed ID3 payload or `duration` tag                                              | item fault           | entry dropped with a warning; not counted                                              |
| `seek` (§12)            | container seek failure; seek after a discontinuity                                          | item fault           | warning, returns 0; not counted                                                        |

Faults in the data are contained to the smallest unit that holds them: an
item is dropped with a warning when decoding can continue without it; the
input ends only when it cannot. Nothing raises out of `decode`, `seek` or
`eof` while the input streams: a source fault is a result of `decode`, read
by the caller.

A source fault reaches the script as [faults.md §3](faults.md#3-source-faults-reach-the-script)
states: the decoder ends the track at the fault, logs the error at error
level, and drains every stage except the one that failed
([draining.md §5](draining.md#5-discarded-on-purpose)), so content decoded
before the fault reaches the output. The decoder never invents content. The
decoder is not a source: what follows the end of track (not ready for a
fallible source, a hard error for an infallible one) is its caller's, from
the source's type (a request-based source, `input.ffmpeg`, the harbor
input; [io.md](io.md) for the last two). A decoder fault never stops an
output downstream.

Error messages above are descriptions; their wording is not normative.

## 14. Cue in and cue out

Cue points belong to the request layer, which wraps the file decoder:

- Cue in: when the request's cue-in is positive, the request seeks once by
  that many ticks before the first read, and logs a mismatch when the result
  differs. This decoder returns the requested value (§12), counted from the
  container origin.
- Cue out: the request stops reading at the cue-out position counted in
  delivered ticks, slices the last frame, and caps the remaining time and
  seeks to it. The decoder takes no part.

Example: `annotate:liq_cue_in=1.2,liq_cue_out=2.5:a.mp3` plays 1.3 s.

## 15. Interfaces reused elsewhere

| Interface                                                           | Used by                                                                        | Contract                                                                                                                                                                                          |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| content type from an open container (§5.2), logging the recognition | `input.ffmpeg` ([io.md](io.md))                                                | takes an optional forced format, the requested content type, the URL for logs, and the container                                                                                                  |
| decoder record from an open container                               | `input.ffmpeg`, harbor input ([io.md](io.md))                                  | takes the requested content type, `decode_first_metadata`, and the container; offers `decode`, `eof`, `seek`, `close` (§8, §13, §12; `close` closes the container) and the remaining time (§11.2) |
| stream configuration (§5.1)                                         | `input.ffmpeg`                                                                 | passed to `open_input`                                                                                                                                                                            |
| internal video converter (§16.1)                                    | internal video decoder, inline decoders ([inline-codecs.md](inline-codecs.md)) | §16.1                                                                                                                                                                                             |
| subtitle timeline (§16.2)                                           | text and copy subtitle decoders                                                | §16.2                                                                                                                                                                                             |

Clauses other files rely on ([io.md](io.md)): `eof` drains the record as
§13.1 states; `close` closes the container and releases the record without
draining; `decode` reports end of input and an abort distinctly; the
content-type interface works on any open container, including one opened
after a harbor wake-up.

The file decoder uses `decode_first_metadata = false`; the stream decoder,
`input.ffmpeg` and the harbor use `true`.

## 16. Shared helpers

### 16.1 Internal video converter

Turns decoded video frames into internal video images of a fixed size. Used
by the internal video decoder
([decoder-internal.md](decoder-internal.md)) and the inline decoders
([inline-codecs.md](inline-codecs.md)). Built with a target width `W`,
height `H`, and a function giving the target pixel format for a frame
format. It holds at most one **stage**: a stage is built for one frame
format, one time base and one stream index.

`convert` takes a frame, its time base, optionally a stream index (default 0) and a format (default the frame's own), and a `put` function:

1. When the current stage has the same stream index, the same time base and a
   format equal to the given one by the shared video-format comparison
   ([common.md §9](common.md#9-video-format-comparison)), the stage is reused.
2. Otherwise the current stage is drained (as `flush`), a format change is
   logged at important level when a stage existed, and a new stage is built:
   - fitted size `(fw, fh)`: the largest size inside `W × H` that shows the
     frame format with its proportions, square destination pixels
     ([filters.md §16.4](filters.md#164-fit));
   - a scaler from the frame size and pixel format to `fw × fh` and the
     target pixel format, using the configured scaling threads;
   - a frame rate converter for the format and time base to the video rate
     setting ([filters.md §16.2](filters.md#162-fps)).
3. The frame goes through the frame rate converter; each output frame is
   scaled, wrapped as an image, translated by `((W − fw) / 2, (H − fh) / 2)`
   (integer division), clipped to a `W × H` viewport, and given to `put` with
   the frame it came from.

`flush put` drains the current stage's frame rate converter through the same
scaling into `put`. The stage stays usable.

A mid-stream change of frame format is handled without a track mark and
without ending the track: the new stage fits the new format into the same
`W × H`.

Conformance:

- A file whose video changes resolution mid-stream decodes without a track
  mark, every input frame represented, and the images keep `W × H`.

### 16.2 Subtitle timeline

Turns timed subtitle items into chunks on a sparse field. Used by the text
subtitle decoder ([decoder-internal.md](decoder-internal.md)) and the copy
subtitle decoder ([decoder-copy.md](decoder-copy.md)). Built with:

- `process`: item → `(position, duration, data)`, positions in main ticks on
  the field (§8.3);
- `merge`: combines the data of two items at one position;
- `delay`: shifts an item's data `d` ticks later in its own terms (§16.2.1);
- `output`: puts one chunk of a given length on the field, holding some data
  at offset 0, or none.

State: none, or `(p, pending)` with `p` the position reached on the field and
`pending` an optional `(duration, data)` placed at `p`.

On an item, with `(q, d, data) = process item`:

1. No state: when `q > 0`, outputs no data with length `q`; the state becomes
   `(max(q, 0), (d, data))`.
2. State `(p, pending)` with `q > p`: outputs `pending`'s data (or none) with
   length `q − p`; the state becomes `(q, (d, data))`.
3. State `(p, pending)` with `q = p`: the item joins `pending`, data merged,
   duration the larger.
4. State `(p, pending)` with `q < p`: the item arrived after the field was
   filled past its position (an `advance` from other streams went further).
   When `q + d ≤ p` the item has already ended: an item fault, dropped with
   a warning and counted toward `max_consecutive_faults`, since it carries
   media ([faults.md §4](faults.md#4-item-faults-escalate)).
   Otherwise it is delayed by `p − q` (its end kept) and joins `pending` as in
   step 3.

On `advance r`:

1. No state: outputs no data with length `r`; the state becomes `(r, none)`.
2. State `(p, _)` with `r ≤ p`: nothing.
3. State `(p, pending)`: outputs `pending`'s data (or none) with length
   `r − p`; the state becomes `(r, none)`.

On `flush`: when a pending `(d, data)` exists, outputs `data` with length
`max(d, 1)`; the state becomes `(p + max(d, 1), none)`.

A subtitle is thus held at its position until the next item or the next
advance, and its chunk spans the gap to that point.

#### 16.2.1 Delay

What delaying means is the user's: text subtitles shorten their start and end
display times, keeping the end
([decoder-internal.md §4.3](decoder-internal.md#43-text-subtitles)); copied
packets keep their own timestamps, which the encoder uses
([decoder-copy.md §4.2](decoder-copy.md#42-subtitles)).

Conformance:

- Two cues with the same start time both reach the output.
- A subtitle stream whose first cue is at 20 min keeps it at 20 min.
