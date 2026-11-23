# Findings: Ogg encoder

Kept out of the as-built text ([ogg.md](../ogg.md) and the codec files).
Each finding is marked **checked** (something was run; what) or **read only**.
Runs used the binary of this tree (`02fa52985`) and the scripts and dumps in
the agent's scratch directory (a page parser, `output.file` runs).

## Defects

### D1. Opus pre-skip is a constant 3840, not the encoder delay: 73.5 ms of audio lost per link

`Opus.Encoder.create ?(pre_skip = 3840)` (xiph/opus/opus.ml:163); Liquidsoap
never passes it (opus_encoder.ml:44) nor queries the look-ahead. A decoder
discards 3840 samples (80 ms) at the start of every link while the encoder
delay is 312 samples: 3528 samples of real audio are dropped at each link
start, so at every metadata change. RFC 7845 §4.2 has the decoder discard
exactly pre-skip samples.
**Checked**: a 2.6 s sine in two links (1.08 s and 1.5 s of granulepos) decodes
with FFmpeg to 2.42 s, i.e. 2 × 80 ms less. Scenario: a radio stream with a
metadata change per song loses the first 73.5 ms of every song.

### D2. Opus, Speex and Theora end their streams with a "+1" empty page and drop their remainder (contradicts draining.md §4)

They call `Ogg.Stream.terminate` (opus_encoder.ml:116, speex_encoder.ml:145,
theora_encoder.ml:151; stub ogg_stubs.c:288): an EOS page with no segment and
granulepos = last packet + 1. The samples short of a codec frame (Opus FIFO,
Speex carry-over of 1..frame_size samples) and the Opus look-ahead are never
encoded. [draining.md §4](../../../source/spec/draining.md#4-partial-final-units)
requires the final partial unit to be delivered, padded with the padding
signalled. The bindings' own codec EOS functions (`Opus.Encoder.eos`,
`Speex.Encoder.eos`, `Theora.Encoder.eos`) are unused.
**Checked**: dumps show EOS pages `gp=51841`, `72001` (Opus), `66081` (Speex),
`90`, `103` (Theora), each 27 bytes with `nseg=0`; Speex total granulepos
114 560 for 114 660 samples given (Vorbis and FLAC: 114 660). Scenario: every
Opus link ends up to 20 ms + 6.5 ms short; the final granulepos claims one
sample more than the last packet holds.

### D3. A Theora link with no image cannot be ended

theora_encoder.ml:132–150 builds the fallback image with planes of `width`
and `width / 2` bytes declared as `width × height`; the binding's
`yuv_of_val` (theora_stubs.c:225) raises `Theora.Invalid_data` when a plane is
smaller than stride × height. Unused: `Image.YUV420.blank_all yuv` blanks an
image that is then not passed. **Read only.** Scenario: `%ogg(%theora)` (or
with audio) where a metadata change and the next metadata change or the stop
happen with no video image between them (a piece shorter than a video frame
after a metadata change, then stop): `stop` or `encode_metadata` raises.

### D4. Empty-link fallbacks use the frame's channel count, not the codec's

vorbis_encoder.ml:48, opus_encoder.ml:100, ogg_flac_encoder.ml:59 build the
token sample with `Frame.audio_channels`. With `%vorbis(mono)` and the default
stereo frame, the fallback raises `Vorbis.Invalid_channels`; Opus puts a
2-channel piece into a 1-channel FIFO; FLAC feeds libFLAC the wrong channel
count. **Read only.** Scenario: as D3 (a link registered and ended without
data), with a mono codec.

### D5. `bytes_per_page` has no effect on any codec

`ocaml_ogg_stream_pageout` (ogg_stubs.c:320) honours the fill only under
`#ifdef HAVE_PAGEOUT_FILL`, defined by no build rule in the tree (the flag
came in with the vendoring, d772ceee5). The parameter is documented and
parsed for Vorbis, Opus, Speex, FLAC and Theora. **Checked**:
`%opus(bytes_per_page=500)` gives the same 4269-byte pages as `%opus`; grep of
the source and `_build` finds no definition.

### D6. `%vorbis.abr` with its defaults fails at the first encode

lang_vorbis.ml:63 defaults all three ABR rates to unset; vorbis_encoder.ml
passes -1000 for each; libvorbis rejects a managed setup with no rate.
**Checked**: `output.file(%vorbis.abr, …)` fails with `Invalid vorbis setup
request, e.g. out of range argument` from `create_abr`, the output stops.

### D7. Format values that crash type checking

`%ogg`, `%ogg()`, `%ogg(%mp3)`, `%ogg(%vorbis(channels=0))`,
`%vorbis(channels=7)` reach `type_of_format` with an unsupported channel count
and raise `Audio_layout.Unsupported` out of the type checker (encoder.ml:38–52,
lang_ogg.ml:39–49, lang_vorbis.ml and lang_flac.ml do not validate
`channels`). **Checked**: `print(<each>)` ends with `Error -1: Exception
raised: Audio_layout.Unsupported`, no position.

### D8. Printed forms that do not parse back

- `%theora`, `%ogg(%theora)` print `%ogg()`: `Ogg_format.to_string`
  (ogg_format.ml:41–44) returns `[]` when there is no audio, dropping the
  video.
- Theora prints empty fields for unset options (`…,keyframe_frequency=64,,soft_target=false,,)`).
- `%vorbis.abr` prints `bitrate=128,,` for unset rates.
- Opus prints `complexity="3"` (a string; the parser takes an int).
- Speex prints `mode=widebande` (typo) and unquoted modes.

**Checked** with `print(...)`. Scenario: anything that relies on the printed
format (logs, `--list-…`, round trips) shows the wrong or an unparsable format.

### D9. Leading empty skeleton link

With audio and video, `encode_metadata` before the first `encode`
(ogg_encoder.ml:145 → `Ogg_muxer.end_of_stream` → `eos` → `streams_start`
with zero tracks) emits a skeleton-only link (fishead BOS + skeleton EOS)
before the real first link. **Checked**: `%ogg(%vorbis,%theora)` over a
tagged mp3: 120 bytes of skeleton-only link first; the installed ffprobe then
reports `Changing stream parameters in multistream ogg is not implemented`.
Every AV file output of a playlist starts this way.

### D10. Old normative encoder.md: `header()` is not complete after a link change

The previous `encoder.md` §3 said `header()` "after a new logical stream began
… the new stream's header. It MUST be complete … the container's
identification headers included". Between `encode_metadata` and the next
`encode`, the header buffer holds only the new link's BOS pages: the
secondary headers come with the lazy start (ogg_encoder.ml:131–134). And the
bytes the next `encode` returns start with the previous link's EOS pages.
**Read only.** Scenario: a harbor listener joining in that window with
`header()` prepended gets BOS pages, then the old link's EOS pages, then the
new link's secondary headers duplicated.

### D11. Muxer mutates its track table while iterating it

`flush` (ogg_muxer.ml:393–395) iterates with `Hashtbl.iter` calling
`end_of_track`, whose `add_available` may `Hashtbl.remove` the current entry
(:344). The OCaml manual leaves this unspecified; the outer `while` loop
hides missed entries. **Read only.**

### D12. Opus bitrate error message range

`Opus bitrate should be in 5..512` (lang_opus.ml:83) while the check
(`i × 1000` in 500..512000) accepts 1..512. **Read only.**

## Asymmetries

- **End of stream**: Vorbis and FLAC end with an exact final granulepos on
  their last data page; Opus, Speex and Theora with an extra empty page at
  last + 1 (D2).
- **Empty link token**: Vorbis and FLAC encode one silent sample; Opus puts
  one sample that encodes nothing (the link has headers only); Speex encodes
  nothing; Theora raises (D3).
- **Comments**: Vorbis adds `ENCODER=Liquidsoap/<version> (…)` and orders
  comments by hash-table iteration; Theora adds `ENCODER=ocaml-theora by the
savonet team …`; Opus and FLAC add nothing, list order; Speex forces
  `title` first, deriving it from `uri` or `Unknown`. **Checked** in dumps.
- **Fisbone**: every codec but Opus has one; an `%ogg(%opus,%theora)`
  skeleton describes only the video.
- **Page formation**: Vorbis, Opus, Theora let the muxer assemble pages;
  Speex forms pages inside its binding; FLAC delegates the whole Ogg layer to
  libFLAC and leaves the muxer's stream empty (marked ended by a private
  stub).
- **Bitrate**: the instance always answers none, while the format carries
  nominal values that icecast2.ml reads directly from the format.

## Gaps

- **Interleaving stalls on a sparse stream** (ogg_muxer.ml:302–344). A track's
  pages are held until every other track holds one. With a still image,
  Theora completes a page every 255 frames; the audio waits ≈10 s.
  **Checked**: `%ogg(%vorbis,%theora)` with `output.file(flush=true)` stays at
  its 7811 header bytes from 2 s to 10 s; audio alone grows every 1–2 s.
  Nothing bounds this latency; live outputs (harbor, icecast, SRT) see it.
- **Serial uniqueness** is not ensured within a link or across links
  (ogg_muxer.ml:116), and the generator is a global `Random.State` shared by
  all instances, unsynchronised across domains. **Read only.**
- **Opus DTX drops packets without advancing granulepos**
  (opus_stubs.c:794–798): with `dtx=true`, silence makes the granule clock
  fall behind real time, and the muxer's interleaving uses that clock.
  **Read only.**
- **Opus header buffer shared** (opus_stubs.c:494, `static unsigned char
header_packet[19]`): two Opus encoders created at once in two domains race.
  **Read only.**
- **Resampler not drained**: the per-instance resampler (ogg_encoder.ml:38) is
  never flushed at `stop`; with differing rates (Opus at 48 kHz from a 44.1 kHz
  frame) its held samples are lost. **Read only.**
- **Zero-length audio block to Vorbis** calls `vorbis_analysis_wrote(0)`,
  which libvorbis takes as end of input. `output.ml` never encodes an empty
  piece, but the resampler can return 0 samples for a short non-empty piece
  when rates differ (`%vorbis(samplerate=48000)` at 44.1 kHz). **Read only.**
- **Theora size vs frame size**: the format's width/height describe planes of
  images rendered at the frame size (theora_encoder.ml:94–115); a smaller
  frame raises, a larger one is read with the wrong geometry. **Read only.**
- **libFLAC init result unchecked** (flac_ogg_stubs.c:70). **Read only.**
- **No missing-module check for Theora**: `%theora` parses without the Theora
  module and the factory call raises `Invalid_argument "option is None"`
  (ogg_encoder.ml:93). **Read only.**
- **Single-track lag**: with one track the muxer releases a completed page
  only at the next codec call (ogg_muxer.ml:306–312). **Read only.**
- **Second audio codec and unknown arguments of `%ogg` are ignored**
  silently (lang_ogg.ml:39–48). **Checked**: `%ogg(%vorbis, %opus)` prints as
  `%ogg(%vorbis(…))`.
- **Speex defaults** pair narrowband mode with a 44.1 kHz rate
  (lang_speex.ml:28–32). **Checked** in the header dump (rate 44100, mode 0,
  frame size 160).
- **`restart_offset` reports only the latest link start in a return**: two
  metadata changes with no `encode` between leave the earlier link
  unreported (ogg_muxer.ml:187). Not reachable through `output.ml`, which
  encodes a non-empty piece after every metadata. **Read only.**
- **No test** covers header layouts, granulepos, trimming, Speex, Theora,
  the skeleton, `header()` after a metadata change, or interleaving.

## To verify

- RFC 7845: whether a final granulepos one sample beyond the last packet's
  end is valid, and how decoders treat it (end trimming is defined only for
  a final granulepos lower than the decoded total).
- RFC 3533: validity of a page with zero segments carrying a positive
  granulepos (granulepos is meant to be -1 when no packet ends on the page).
- `SPEEX_SET_ABR`: libspeex's control takes a bit rate in bits per second;
  speex_encoder.ml:51 sets it to 1, then `SPEEX_SET_BITRATE`. Whether the
  later call overrides the ABR target, or ABR targets 1 b/s, is unchecked.
- libogg `ogg_stream_pageout`: page threshold of about 4096 body bytes or 255
  lacing values (from memory; consistent with the observed 4.2 KB pages).
- Whether upstream ocaml-ogg defines `HAVE_PAGEOUT_FILL` in its own build.
- The Opus encoder look-ahead of 312 samples at 48 kHz for the `audio`
  application (from memory; the 73.5 ms figure of D1 depends on it, the
  80 ms per link loss does not).
