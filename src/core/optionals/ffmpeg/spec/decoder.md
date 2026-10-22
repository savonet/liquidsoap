# FFmpeg file and stream decoder

This file owns the FFmpeg decoder that Liquidsoap uses for files and
byte streams: which files and MIME types it accepts, how it opens and probes a
container, which content type a file decodes to, how container streams map to
frame fields, the decode loop, positions, seeking, remaining time, metadata,
duration resolution and end of stream. It also defines the **per-stream
decoder contract** (§7) that the three per-stream decoder families implement,
each in its own file with the same numbering:
[decoder-internal.md](decoder-internal.md), [decoder-raw.md](decoder-raw.md),
[decoder-copy.md](decoder-copy.md). The timed ID3 data-stream decoder does not
follow that contract; it is described in §10.3. The image
decoder is in [image-decoder.md](image-decoder.md). It relies on the FFmpeg
bindings ([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md)
for `open_input`, `read_input`, `seek`), on the content types of
[content.md](content.md), on the video helpers (display, frame rate, audio
format, fit) of [filters.md §16](filters.md#16-helper-graphs), and on the encoder timestamp
rules of [timestamps.md](timestamps.md). Generic decoder and request machinery
is stated only where this decoder relies on it.

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

Script-visible builtins generated from these registrations (by generic code):
`request.duration.ffmpeg` and `file.metadata.ffmpeg`.

`request.duration.ffmpeg` (from the implementation, `liquidsoap -h`):

```
Type: (?resolve_metadata : bool, ?metadata : [string * string], ?timeout : float?,
 string) -> float?
```

- `metadata` (default `[]`): "Optional metadata used to decode the file, e.g.
  `ffmpeg_options`." It is merged into the request's metadata and reaches the
  resolver of §11.1, which reads `ffmpeg_options` from it (§4).
- `resolve_metadata` (default `true`), `timeout` (default `null`).

`file.metadata.ffmpeg` has type `(string) -> [string * string]`: it calls the
resolver of §10.4 with no request metadata, lowercases every key and returns
`[]` when the resolver raises.

## 2. Settings

| Key                                                    | Type        | Default                       | Meaning                                                    |
| ------------------------------------------------------ | ----------- | ----------------------------- | ---------------------------------------------------------- |
| `settings.decoder.ffmpeg.max_interleave_duration`      | float       | `5.0`                         | seconds kept while waiting for all streams (§8.4)          |
| `settings.decoder.ffmpeg.codecs.<codec>`               | string      | unset                         | preferred FFmpeg decoder name for codec `<codec>` (§5.1)   |
| `settings.decoder.ffmpeg.codecs.<codec>.available`     | string list | FFmpeg decoders for `<codec>` | informative list of decoder names                          |
| `settings.decoder.mime_types.ffmpeg`                   | string list | 71 entries (§3.4)             | MIME types for media                                       |
| `settings.decoder.mime_types.ffmpeg.images`            | string list | 5 entries (§3.4)              | MIME types for images                                      |
| `settings.decoder.file_extensions.ffmpeg`              | string list | 290 entries (§3.4)            | extensions for media, images excluded                      |
| `settings.decoder.file_extensions.ffmpeg.images`       | string list | 58 entries (§3.4)             | extensions for images                                      |
| `settings.decoder.priorities.ffmpeg`                   | int         | `10`                          | priority of the media decoder and of the duration resolver |
| `settings.decoder.priorities.image.ffmpeg`             | int         | `10`                          | priority of the image decoder                              |
| `settings.request.metadata_decoders.priorities.ffmpeg` | int         | `1`                           | priority of the metadata resolver                          |

`<codec>` ranges over the codec identifier names (as FFmpeg prints them) of
every audio and video decoder FFmpeg provides. The `available` list holds the
decoder names for that codec, the last registered first. Example (from the
implementation): `settings.decoder.ffmpeg.codecs.mp3.available()` is
`["mp3", "mp3float"]`. Subtitle codecs get no key.

All lists and priorities are read at each use, so changing them at runtime
takes effect for the next file.

## 3. Acceptance and priority

### 3.1 Files

A file is offered to this decoder by the generic file-decoder selection:

1. The extension is the part after the last dot, lowercased. The MIME type is
   looked up from the file name.
2. The file passes when its extension is in
   `file_extensions.ffmpeg @ file_extensions.ffmpeg.images`, **or** the part of
   its MIME type before the first `;` equals the part before the first `;` of
   one entry of `mime_types.ffmpeg @ mime_types.ffmpeg.images`. A failed test
   logs, at level 4, `Unsupported file extension for "<file>"!` and
   `Unsupported MIME type for "<file>": <mime>!`.
3. Passing decoders are tried in decreasing priority (stable for equal
   priorities, in the order of `settings.decoder.decoders`). For each, the
   file type probe (§5) gives a content type; the first one whose content type
   can be converted to the requested one wins. "Can be converted" keeps only
   the requested fields, accepts an audio field whose channel layout can be
   converted, and then checks compatibility.

With the default settings the decoders available for a file are listed as
`ffmpeg (priority: 10), image (priority: 1)` (from the implementation, for a
`.mkv` and a `.png`).

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
endless video track. Example (from the implementation): a 64×48 PNG
requested as `source(video=ffmpeg.video.raw)` was still playing after 3 s.

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

| Parameter                                 | Result                                                                                                     |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| `f=<identifier>` or `format=<identifier>` | forced input format: the demuxer named `<identifier>`; an unknown name is ignored and the format is probed |
| `<key>=<identifier>`                      | option `<key>` with the identifier as a string                                                             |
| `<key>="<string>"`                        | string option                                                                                              |
| `<key>=<int>`                             | integer option                                                                                             |
| `<key>=<float>`                           | float option                                                                                               |
| anything else, or a parse error           | `Runtime_error` of kind `ffmpeg_decoder`, message `Invalid mime type arguments!`                           |

A later parameter with the same key replaces the earlier one. Absent
metadata means no options and no forced format.

Example (from the implementation): `annotate:ffmpeg_options="foo=1,f=mp3":a.mp3`
is probed and accepted, then fails at decoder creation with
`Runtime_error { kind: "ffmpeg_decoder", msg: "Unrecognized options: foo=1" }`.

### 4.2 `application/ffmpeg` MIME parameters

A stream whose MIME type, split on `;`, starts with `application/ffmpeg` takes
the remaining parts, rejoined with `;`, as an option string parsed as in §4.1.
Any other MIME type gives no options.

### 4.3 Image options and unconsumed options

For a file whose extension (as returned by the platform's extension function:
the dot included, case kept) is `.` followed by an entry of
`file_extensions.ffmpeg.images`, and for a stream whose full MIME type equals
an entry of `mime_types.ffmpeg.images`, the options `loop = 1` and
`framerate = <video rate in frames per second>` are set, replacing any given
ones.

Opening a container removes from the option table the options the demuxer
consumed. When the file or stream decoder is created and options remain, it
raises `Runtime_error` of kind `ffmpeg_decoder` with message
`Unrecognized options: <options>`, the options rendered as by the encoder
([encoder.md](encoder.md)). The probe (§5), the duration resolver (§11.1) and
the metadata resolver (§10.4) do not check remaining options.

## 5. Opening and probing

### 5.1 Opening

The probe and the decoders open the container with the bindings'
`open_input` (files) or `open_input_stream` (streams), with the forced format
and the options of §4. The probe and the decoders pass a stream configuration
callback per media type that picks the preferred decoder:

1. Take the codec identifier name of the stream's parameters.
2. When a `settings.decoder.ffmpeg.codecs.<name>` key exists and is set, log at
   level 4 `Trying preferred decoder <dec> for codec <name>` and look the
   decoder up by name.
3. A failed lookup logs at level 2 `Failed to set decoder <dec> for codec
<name>: <exn>` and falls back to FFmpeg's default decoder. An unset key or
   no key gives the default decoder.

The duration and metadata resolvers open without this configuration.

The stream decoder passes a seek callback when the input has one: FFmpeg's
`(offset, whence)` becomes a call to the input's absolute seek with `offset`,
whatever `whence` is.

### 5.2 Container description and content type

The probe opens the container (§5.1), describes it and derives the content
type it yields for the requested one, as
[stream-description.md §2](stream-description.md#2-container-format-url-input),
[§5](stream-description.md#5-describe) and [§6](stream-description.md#6-get_type-ctype-container)
specifies: field numbering, entry formats, the specific format name, the
per-stream decoded formats, and the failure `No valid stream found in
container.`. Between the description and the content type, the decoder logs
at level 3 `FFmpeg recognizes "<uri>" as <description>`. The container is
closed afterwards, whatever the outcome.

Errors raised by the probe (open failure, unification failure) are logged by
the generic selection at level 2 as `Error while checking file's content:
<exn>`, and the next decoder is tried.

Log lines of one probe (from the implementation, a Matroska file with AAC
mono, H.264 and SRT, requested as audio, video and subtitles):

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
per-stream decoder. Fields are numbered per media type over **all** streams
of that type in container order, whether or not a stream is selected or
readable; field names follow [stream-description.md §3](stream-description.md#3-fields)
(`audio`, `audio_2`, …). The probe (§5.2) numbers only the streams whose
parameters were read successfully
([stream-description.md §3](stream-description.md#3-fields)), so after a stream that
fails there, the content type the probe returns and the fields this mapping
fills name different streams. A stream is selected when the
requested content type has its field; the requested format picks the decoder.

| Stream        | Requested format           | Read as | Per-stream decoder                                                                         | Sparse                  |
| ------------- | -------------------------- | ------- | ------------------------------------------------------------------------------------------ | ----------------------- |
| audio         | copy                       | packets | copy audio ([decoder-copy.md](decoder-copy.md))                                            | no                      |
| audio         | raw audio                  | frames  | raw audio ([decoder-raw.md](decoder-raw.md))                                               | no                      |
| audio         | PCM (float, s16, f32)      | frames  | internal audio, channel count from the format ([decoder-internal.md](decoder-internal.md)) | no                      |
| video         | copy                       | packets | copy video                                                                                 | no                      |
| video         | raw video                  | frames  | raw video                                                                                  | no                      |
| video         | internal video             | frames  | internal video                                                                             | no                      |
| subtitle      | copy                       | packets | copy subtitles                                                                             | yes                     |
| subtitle      | text subtitles             | frames  | internal text subtitles                                                                    | yes                     |
| subtitle      | internal video             | frames  | internal bitmap subtitles                                                                  | yes                     |
| data          | — (codec `timed_id3` only) | packets | timed ID3 (§10.3)                                                                          | yes, nothing to advance |
| anything else |                            |         | not read                                                                                   |                         |

For internal video, before the decoder is built, the stream's expected
display size ([side-data.md §3](side-data.md#3-autorotate-on-decoding)) is offered to the frame
size negotiation as the ideal size with source name `ffmpeg decoder`; the
decoder then uses the width and height of the requested format, and the
format's alpha is set from the stream's pixel format when not yet decided.

Every decoder created for one container shares one fresh stream index
([content.md §5](content.md#5-stream_idx-and-time_base)), carried by copy and raw content.
A seek builds the whole mapping again, with a new stream index (§12).

Audio and video streams in all modes are wrapped so that, before each item is
decoded, the stream's metadata is read (§10.2) and the remaining time is
updated (§11.2). Subtitle and data streams are not wrapped.

A data stream whose parameters raise an FFmpeg error is skipped with
`Failed to get stream info: <exn>` at level 2.

## 7. Per-stream decoder contract

Each per-stream decoder implements this contract. Implementation files use
the same numbers, §1 to §8.

### 7.1 Selection

The row of the table in §6 that creates it, and what it needs at creation:
the stream, its codec parameters, the field, the requested format, and for
copy and raw the stream index.

### 7.2 Construction

What it computes once at creation, and what it raises when the stream does
not fit the requested format. A raise aborts the decoder creation and the
file is not played.

### 7.3 Input

A decoder receives, one call at a time, items of one kind: decoded frames, or
packets, or subtitle frames, and a final `Flush`. Items come in demuxing
order. The decode loop (§8) decides which items reach the real output and
which are decoded into a throwaway output.

### 7.4 Output

What it writes to the output: content chunks put on its field, metadata added
at the current position. Output position is implicit: each field grows by the
length of what is put, and metadata is added at the current length of the
output.

### 7.5 Timestamps

Which timestamps it reads, their time base, how lengths are derived, how gaps
and non-monotonic values are handled, and which timestamps it leaves in the
content.

### 7.6 Flush

What `Flush` (end of input, §13) emits.

### 7.7 Sparse advance

Sparse decoders (subtitles, timed ID3) receive, besides items, an `advance
p` call with `p` the minimum position, in main ticks relative to each
stream's first timestamp, reached by the non-sparse streams (§8.3). It lets
them fill their field up to `p` when no item arrives.

### 7.8 Errors and logs

What it raises, what it drops, what it logs.

## 8. Decode loop

### 8.1 State

Per decoder record (one per opened container, §15):

- the stream map: container stream index → per-stream entry (§6) with its
  time base, sparse flag, `seen` flag, `first_position`, `pts` and `position`
  (all in main ticks), rebuilt on seek;
- `target_position`: none, or the seek target in main ticks (§12);
- `remaining` (§11.2);
- a lock shared by decoding and seeking;
- the last container metadata emitted (§10.1);
- the pending list of the interleaving buffer (§8.4) and the discard output,
  created at first use (§8.5).

Main ticks are those of [common.md §4](common.md#4-liquidsoap-time-bases).

### 8.2 One decode step

The caller (the generic file decoder, `input.ffmpeg`, the harbor) calls
`decode` repeatedly until its output holds enough data. One call:

1. Takes the lock.
2. Reads the container metadata; emits it when it differs from the last one
   emitted and is not empty (§10.1).
3. Reads one item with the bindings' `read_input`, passing the selected
   streams in their read mode ([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md), `read_input`).
4. Takes the item's stream index and two timestamps: for a packet, its DTS
   (bookkeeping) and PTS (position); for a frame or subtitle frame, its PTS for
   both. An item of a stream not in the map makes the call read again.
5. Runs the position check (§8.3–§8.5) with the item, which decodes it into
   the real or the discard output, or defers it.
6. Errors of `read_input`: `Eagain` and `Invalid_data` make the call read
   again; `Exit` and `Eof` raise the decoder's end-of-file exception; any other
   exception propagates with its backtrace.

### 8.3 Positions and sparse advance

For an item with a PTS `t` in the stream's time base:

1. `ticks` is `t` converted from the stream time base to main ticks
   ([common.md, Time-base conversion](common.md#5-time-base-conversion)).
2. The stream's `pts` becomes `ticks`.
3. On the stream's first timestamped item, `first_position = ticks` and
   `position = 0`. On later items, `position = ticks − first_position`.
4. After a later item of a non-sparse stream, the minimum `position` over
   non-sparse streams that have one is computed. When it is greater than the
   last value advanced, every sparse stream receives `advance` with it (§7.7).

An item with no PTS changes nothing here.

### 8.4 Waiting for all streams

Decoding into the real output starts only once every non-sparse stream has
produced a timestamped item. The reason, from the change that introduced it:
a live source can carry a long stretch of audio before the first video frame,
which desynchronised the start.

For an item with `pts` (absolute ticks) at or after the target (§8.5):

1. The stream is marked seen.
2. When every non-sparse stream is seen, the pending list is flushed (below)
   with the item's `pts`, then the item is decoded into the real output.
3. Otherwise the item is pushed: it joins the pending list as
   `(pts, ts, decode)`, with `ts` the bookkeeping timestamp of §8.2 (0 when
   absent), and every pending entry whose `pts` differs from the new one by
   more than `max_interleave_duration` (in ticks: 5 s → 220500) is dropped
   without being decoded.

Flushing the pending list with position `P`:

1. When a target is set, entries with `pts` below it are dropped.
2. The rest is sorted by `ts`, raw values compared whatever the stream's time
   base.
3. Entries with `pts ≥ P − max_interleave_duration` are decoded into the real
   output in that order; the others are dropped.
4. The list is emptied.

An item with no PTS bypasses all of this: it logs at level 3 `Got packet or
frame with no timestamp! Synchronization issues may happen.` and is decoded
into the real output at once.

### 8.5 Pre-roll after a seek

While a target is set, an item whose stream `pts` is below the target is
decoded into the discard output: a second output of the same content type,
created on first use and emptied before each such item. The item does not
mark its stream seen. The reason, from the code: codecs that carry state
across packets (mp3 and its bit reservoir) produce up to about 150 ms of
silence when decoding starts at the seek point; decoding the preceding
packets and throwing the result away brings them back to speed. The discard
output is separate because truncating the real output would also shrink its
metadata and track-mark contents.

The target is not cleared once reached: it stays for the life of the stream
mapping (until the next seek).

## 9. Timestamps on the decoding side

| Timestamp                | Time base                                  | Used for                                                                              |
| ------------------------ | ------------------------------------------ | ------------------------------------------------------------------------------------- |
| packet DTS               | stream                                     | pending-list ordering (§8.4); copy chunk lengths ([decoder-copy.md](decoder-copy.md)) |
| packet PTS               | stream                                     | positions (§8.3); copy subtitle positions                                             |
| frame PTS                | stream (frames from `read_input`)          | positions; raw lengths; internal video frame rate conversion                          |
| subtitle frame PTS       | FFmpeg internal, `1/1000000`               | positions (§8.3, converted with the stream time base); internal subtitle placement    |
| subtitle display times   | milliseconds, relative to the subtitle PTS | internal subtitle start and end                                                       |
| packet or frame duration | stream                                     | remaining time (§11.2); copy flush length                                             |

All conversions to ticks use the layer's integer conversion
([common.md, Time-base conversion](common.md#5-time-base-conversion)), truncating toward zero. None
of them rebases on the container start time: positions in §8.3 are relative
to each stream's first timestamp, while the stream `pts`, the seek target and
the remaining time use absolute timestamps.

### 9.1 Grouping by timestamp increments

Copy and raw decoders cut their items into content chunks with the layer's
duration helper ([common.md, Duration helper](common.md#6-duration-helper)): DTS mode for
packets, PTS mode for frames, source time base the stream's (the audio format
stage's for raw audio), offset 0, no initial last timestamp, and
`convert_ts = false`, so items keep their timestamps in the stream time base.
Consequences for the decoders:

- a chunk is emitted when an item with a larger timestamp arrives; it holds
  the previously held items and is as long as that step, so content lags the
  timestamps by one item;
- items with equal timestamps, an increment under one tick, or no timestamp
  travel in the held group;
- a decreasing timestamp raises the content's `Invalid` exception, which ends
  the track (§13.2);
- each step is truncated to whole ticks separately, so lengths add up to less
  than the timestamp span.

## 10. Metadata

### 10.1 Container metadata during decoding

At every decode step (§8.2, step 2) the container metadata (all key/value
pairs FFmpeg holds for the container) is compared with the last emitted list;
when it differs and is not empty, it is added to the output at its current
position. The first decode step of every decoder emits the container metadata
when there is any.

### 10.2 Stream metadata during decoding

Before each item of an audio or video stream, that stream's metadata is read
and then cleared in FFmpeg. The clearing works around memory growth in FFmpeg
with chained Ogg streams. The read metadata is added to the output when all
hold:

- it is not empty;
- it differs from the last list added for this stream;
- either `decode_first_metadata` is true for this decoder record, or a list
  has already been added for this stream.

`decode_first_metadata` is false for files and true for streams,
`input.ffmpeg` and the harbor (§15). With false, the third condition never
becomes true, so stream metadata is never added for files.

Decoded audio and video frames may carry metadata of their own; the internal
decoders add it ([decoder-internal.md §4](decoder-internal.md#4-output)).

### 10.3 Timed ID3

A data stream whose codec is `timed_id3` gets a decoder that, for each
packet, parses the payload as ID3v2 and adds the resulting tags to the output
when not empty:

1. A payload shorter than 3 bytes gives no tags.
2. A payload starting with `ID3` is parsed as is.
3. Otherwise the payload is prefixed with the 5 bytes `I D 3 0x03 0x00` and
   parsed; when that fails, with `I D 3 0x04 0x00`.
4. Any parse failure gives no tags.

The prefixing works around FFmpeg ticket 9540, which strips the ID3 header.

### 10.4 Tags for requests (metadata resolver)

Called with the request metadata, the lowercased extension (or none), the
MIME type and the file name:

1. The file must pass the test of §3.1 against the **media** lists only
   (`file_extensions.ffmpeg`, `mime_types.ffmpeg`); otherwise the result is
   empty.
2. The file is opened with the options of §4.1, without the stream
   configuration.
3. The result is the tags of FFmpeg's best audio stream (none when there is
   no audio stream), followed by the container tags, each key `track`
   renamed to `tracknumber` (exact, case-sensitive match).
4. The container is closed.
5. Any error other than the extension test logs at level 2 `Error while
decoding file tags: <exn>` and raises not-found, which the caller treats
   as no tags.

The generic request code lowercases keys, recodes values to UTF-8, and keeps
the first value of a repeated key. With its priority of 1 this resolver is
below the request metadata (priority 5): request metadata wins over its tags.

Examples (from the implementation, `file.metadata.ffmpeg`):

```
a.mp3  → [("artist", "Me"), ("encoder", "Lavf63.7.100"), ("title", "Hello"), ("tracknumber", "3/10")]
av.mkv → [("duration", "00:00:03.023000000"), ("encoder", "Lavf63.7.100")]
```

The Matroska `DURATION` stream tag becomes `duration` (§11.1).

## 11. Duration and remaining time

### 11.1 Duration resolver

Registered with the decoder priority (10) and the **media** extension list
only (no MIME check, no image extensions). Given the request metadata and a
file:

1. Opens the file with the options of §4.1 (no stream configuration, no
   remaining-option check).
2. Reads the container duration in milliseconds, rounded by FFmpeg to the
   nearest unit, and divides by 1000.
3. Closes the container; no duration raises not-found, so the next resolver
   is tried.

Examples (from the implementation): `request.duration.ffmpeg("a.mp3")` is
`3.0`; `request.duration.ffmpeg(resolve_metadata=false, "av.mkv")` is `3.023`;
a PNG gives `null`.

The generic request code checks the `duration`, `cue_in` and `cue_out`
metadata before calling resolvers.

### 11.2 Remaining time

The decoder record computes the remaining time from the container duration
`D` (seconds, as §11.1; none when unknown or when reading it raises):

1. `remaining` starts at `D`.
2. For each item of an audio or video stream (all modes) with a PTS `t` and a
   duration `δ` (0 when absent), in the stream time base `num/den`: `end =
(t + δ) × num / den` seconds, as a float, and `remaining = min(remaining, D
− end)` (or `D − end` when `remaining` is none). Nothing changes when `D`
   is none or `t` is absent.
3. The reported value is `remaining` converted to ticks, or −1 when it is
   none.
4. A seek sets `remaining` to none.

`end` is absolute: for a container whose start time is not zero, the
remaining time is short by that start time and becomes negative before the
end. The generic file decoder adds the length of its buffered output to this
value; −1 stands for "unknown" for the request layer.

Example (from the implementation): an MPEG-TS file with start time 1.4 s and
duration 3.004 s reports `remaining = 1.584` at 0.02 s of playback.

## 12. Seeking

`seek x` skips `x` ticks forward (backward when negative) and returns the
number of ticks it reports as skipped.

1. The current position is `ticks(D) − reported remaining` when the container
   duration `D` is known; otherwise the `position` (§8.3) of the first
   non-sparse stream that has been seen, in container index order. With
   neither, `seek` returns 0 and does nothing.
2. Under the lock: `target = current + x`; logs at level 3 `Setting target
position to <target in seconds, %f>`; sets `target_position`; seeks the
   container with the bindings' `seek` in milliseconds, `ts = max_ts = ⌊max(0,
target_s − 0.5) × 1000⌋` and `min_ts` unbounded, so FFmpeg lands on a
   seekable point at or before 0.5 s ahead of the target.
3. Rebuilds the stream mapping (§6) and all per-stream decoders, with the same
   `decode_first_metadata`, a new stream index, and fresh positions and
   pending list. The previous per-stream decoders are dropped without flush.
4. Sets `remaining` to none.
5. Returns `x`.

Decoding then resumes at the landing point; items before the target go to
the discard output (§8.5), so the first output item is the first one whose
PTS, in ticks, is at or after the target. Accuracy is therefore one decoded
frame or packet: content starts on the first frame boundary at or after the
target, and the reported skip is the requested one.

When the bindings' `seek` raises, the exception propagates; `target_position`
is already set and the mapping is not rebuilt.

The generic file decoder seeks within its buffered output when `0 ≤ x ≤
buffered`; otherwise it empties the buffer and calls `seek (x − buffered)`,
reporting `buffered + result`.

## 13. End of stream and errors

### 13.1 End of input

`read_input` raising `Eof` or `Exit` raises the decoder's end-of-file
exception. The generic file decoder catches every exception from `decode`,
logs at level 2 `Decoding "<file>" ended: <exn>.` with the backtrace at level
4, marks decoding done and calls `eof`. Normal end of file thus logs (from
the implementation):

```
[decoder:2] Decoding "/…/av.mkv" ended: Ffmpeg_decoder.End_of_file.
```

`eof` sends `Flush` to every per-stream decoder of the current mapping (timed
ID3 ignores it), then adds a track mark. Items still in the pending list
(§8.4) are not decoded.

What `Flush` puts goes into the generic file decoder's buffer like any other
content. That buffer adds the track mark at, and delivers up to, the length
of its shortest media field; content of a longer field past that length is
not delivered. Example: in [timestamps.md §11.1](timestamps.md#111-ffmpegformatmpegts-audiocopy-videocopy-video-fed-first), the final
audio packet held by the copy decoder ends after the video, so it is not
delivered.

### 13.2 Errors

| Where               | Error                                 | Effect                                           |
| ------------------- | ------------------------------------- | ------------------------------------------------ |
| option parsing (§4) | `Invalid mime type arguments!`        | decoder creation fails; probe and resolvers fail |
| decoder creation    | `Unrecognized options: …`             | decoder creation fails                           |
| probe, no streams   | `No valid stream found in container.` | decoder rejected for this file                   |
| opening             | FFmpeg error                          | probe or creation fails                          |
| `read_input`        | `Eagain`, `Invalid_data`              | read again                                       |
| `read_input`        | `Eof`, `Exit`                         | end of input (§13.1)                             |
| `read_input`        | other                                 | propagates: track ends as §13.1                  |
| per-stream decoder  | non-monotonic timestamp (§9.1)        | propagates: track ends as §13.1                  |
| close               | —                                     | closes the container                             |

With `settings.decoder.debug` set, the generic file decoder re-raises after
`eof`.

## 14. Cue in and cue out

Cue points belong to the request layer, which wraps the file decoder:

- Cue in: when the request's cue-in is positive, the request seeks once by
  that many ticks before the first read and logs at level 3 `Initial seek
mismatch! Expected: <x>, effective: <y>` when the result differs. This
  decoder reports the requested value (§12), or 0 when it knows no position.
- Cue out: the request stops reading at the cue-out position counted in
  delivered ticks, slices the last frame, and caps the remaining time and
  seeks to it. The decoder takes no part.

Example (from the implementation): `annotate:liq_cue_in=1.2,liq_cue_out=2.5:a.mp3`
plays 1.3 s.

## 15. Interfaces reused elsewhere

| Interface                                                                                 | Used by                                                | Contract                                                                                                                                                                                                  |
| ----------------------------------------------------------------------------------------- | ------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| content type from an open container (§5.2), logging the `recognizes` line                 | `input.ffmpeg` ([io.md](io.md))                        | takes an optional forced format, the requested content type, the URL for logs, and the container                                                                                                          |
| decoder record from an open container                                                     | `input.ffmpeg`, harbor input ([io.md](io.md))          | takes the requested content type, `decode_first_metadata`, and the container; returns `decode`, `eof`, `seek`, `close` (§8, §13, §12; `close` closes the container) and the remaining-time reader (§11.2) |
| stream configuration callbacks (§5.1)                                                     | `input.ffmpeg`                                         | passed to `open_input`                                                                                                                                                                                    |
| internal video converter ([decoder-internal.md §2](decoder-internal.md#2-construction))   | inline decoders ([inline-codecs.md](inline-codecs.md)) | `convert ?stream_idx ?format ~time_base ~put frame`, `flush ~put ()`                                                                                                                                      |
| subtitle timeline helper ([decoder-internal.md §7](decoder-internal.md#7-sparse-advance)) | text and copy subtitle decoders                        | `process`, `output`, `advance`, `flush`                                                                                                                                                                   |

The file decoder uses `decode_first_metadata = false`; the stream decoder,
`input.ffmpeg` and the harbor use `true`.
