# Ogg Vorbis stream encoder

Answers [ogg-streams.md](ogg-streams.md) for Vorbis (libvorbis through the
vendored binding). Container behaviour is [ogg.md](ogg.md).

## 1. Format and options

Three registered names, each also usable alone (`%vorbis` ≡
`%ogg(%vorbis)`):

| name          | mode                    | defaults             |
| ------------- | ----------------------- | -------------------- |
| `%vorbis`     | VBR, quality            | `quality=0.3`        |
| `%vorbis.cbr` | CBR, one bitrate        | `bitrate=128` (kbps) |
| `%vorbis.abr` | managed min/average/max | all three unset      |

Common: `channels=<int>` (default 2, not validated here; a count with no
channel layout fails at type checking, [ogg.md §1](ogg.md#1-format-and-options)),
`stereo`/`mono`, `samplerate=<int>` (default the frame audio rate, kept lazy;
the comment in the code says the channel default is hard-coded "in order not
to force the evaluation of the number of channels too early, see #933"),
`bytes_per_page=<int>` (no effect).

Mode-specific:

- `%vorbis`: `quality=<float>` in [-0.2, 1], else `quality should be in
[(-0.2)..1]`; `quality=<int>` only 0 or 1, else `quality should be in
[-(0.2)..1]`.
- `%vorbis.cbr`: `bitrate=<int>` in kbps.
- `%vorbis.abr`: `bitrate=` (average), `min_bitrate=`, `max_bitrate=`, kbps.

Printed form: `%vorbis(quality=0.30,channels=2,samplerate=44100)`,
`%vorbis.cbr(bitrate=128,channels=2,samplerate=44100)`,
`%vorbis.abr(min_bitrate=…,bitrate=…,max_bitrate=…,channels=…,samplerate=…)`
where each unset ABR rate is omitted but its comma is not, giving
`%vorbis.abr(bitrate=128,,channels=2,samplerate=44100)` (checked), which does
not parse back.

## 2. Instance creation

Per instance: channel count, sample rate (forced), resampler. Per link: a
libvorbis encoder, VBR (`vorbis_encode_init_vbr(channels, rate, quality)`) or
managed (`vorbis_encode_init(channels, rate, max, nominal, min)` with the
kbps values × 1000 and an unset value passed as -1000; CBR passes the bitrate
three times); then the three header packets, built at once from the link's
comments.

## 3. Identification header (BOS page)

The identification packet (30 bytes, `\x01vorbis`…) alone on a flushed page:
58 bytes.

## 4. Skeleton fisbone

80 bytes: 3 header packets, granule rate = sample rate / 1, preroll 2,
granule shift 0, `Content-type: audio/vorbis\r\n`.

## 5. Secondary headers

The comment and setup packets, flushed: one page with both when they fit
(4006 bytes with no metadata at 44.1 kHz stereo quality 0.3).

## 6. Comments

Vendor string: libvorbis's (`Xiph.Org libVorbis I 20200704 (Reducing
Environment)` with the installed library). Comments: every metadata pair as
`key=value`, in the iteration order of a hash table built from the metadata
(unspecified, `title` before `artist` in the example), then
`ENCODER=Liquidsoap/<version> (<OS type>; OCaml <version>)`.

## 7. Data input and conversion

Each block goes straight to libvorbis (`vorbis_analysis_buffer` /
`vorbis_analysis_wrote(len)`), then every block libvorbis has ready is
analysed and its packets put in the stream. libvorbis holds its look-ahead.
A block whose channel count differs from the encoder's raises (§12). A
zero-length block calls `vorbis_analysis_wrote(0)`, which libvorbis reads as
end of input ([findings](findings/ogg.md)).

## 8. Packets, granule positions and pages

Granulepos is libvorbis's: the sample count, at the codec rate, at the end of
each packet. Pages are assembled by libogg's default threshold.

## 9. Page end time

granulepos / codec rate.

## 10. End of stream

If no block was given in the link, one silent sample per channel is encoded
first, with the **frame's** channel count (not the codec's; [findings](findings/ogg.md)).
Then a zero-length block ends the stream: libvorbis flushes its look-ahead
and emits the last packets with the EOS flag and an exact final granulepos;
no extra page. Returns no page (the muxer assembles them). Nothing is lost;
the final granulepos equals the number of samples given (45 864 in
[ogg.md example 1](ogg.md#worked-examples)).

## 11. Page sizing

libogg default threshold; `bytes_per_page` ignored.

## 12. Failures

- channel count of a block ≠ encoder's: binding exception "invalid channels"
  (from `encode` or from the end-of-stream fallback);
- libvorbis init errors at `reset` (invalid quality/bitrate combination for
  the rate and channels): binding exceptions, raised from `encode` (first
  link) or `encode_metadata`. `%vorbis.abr` with none of its three rates set
  (its defaults) always fails this way, at the first `encode`: `Invalid
vorbis setup request, e.g. out of range argument` (checked), since libvorbis
  needs a nominal or a maximum or a minimum rate.

## 13. Concurrency

The analysis loop runs with the OCaml runtime lock released. No global state.

## 14. Tests

`tests/media` (mono, stereo), every harbor/SRT Ogg test (they all use
`%ogg(%vorbis)` but one), `tests/language/type_errors.liq` (channel typing).
