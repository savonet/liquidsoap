# Findings

The agenda of the normative pass: what reading the code closely surfaced,
kept out of the as-built text. One file per area, each sorted into
**Defects**, **Asymmetries**, **Gaps** and **To verify**, each entry marked
**checked** (something was run) or **read only**.

| File                                                 | Area                                                                                  |
| ---------------------------------------------------- | ------------------------------------------------------------------------------------- |
| [seam.md](seam.md)                                   | The seam, formats, typing, the previous normative text.                               |
| [simple-encoders.md](simple-encoders.md)             | `%wav`, `%avi`, `%ndi`, `%external`.                                                  |
| [native-codecs.md](native-codecs.md)                 | `%mp3`, `%shine`, `%fdkaac`, `%flac`.                                                 |
| [ogg.md](ogg.md)                                     | `%ogg`, its muxer and stream encoders.                                                |
| [ffmpeg.md](ffmpeg.md)                               | `%ffmpeg` at the seam and its I/O.                                                    |
| [file-callers.md](file-callers.md)                   | The encoded-output base, `output.file`, `output.external`, `pipe`.                    |
| [net-callers.md](net-callers.md)                     | Icecast, harbor, SRT, `output.url`, HLS.                                              |
| [known-complexity.md](known-complexity.md)           | Entries of the known-complexity list the as-built text does not cover or contradicts. |
| [verification-encoders.md](verification-encoders.md) | Adversarial re-check of the defects of the first four files.                          |
| [verification-callers.md](verification-callers.md)   | Adversarial re-check of the defects of the last three, and FFmpeg V1.                 |

The verification tried to refute every defect: none of the 67 was refuted,
three are worse than first stated and ten were corrected (cause, location
or measure). Where a verification file corrects an entry, the verification
governs.

## Confirmed defects, most severe first

Data loss and outputs that stop or produce nothing:

1. `%ffmpeg` `header()` is empty for codecs with a delay (AAC, LAME MP3): the
   container header leaves with the first `encode` and is never captured, so
   shared harbor listeners and late SRT peers get a stream with no container
   header ([verification-callers V1](verification-callers.md)).
2. `%ffmpeg` MP4 to any stream destination produces nothing, forever, with one
   log line ([ffmpeg F1](ffmpeg.md)).
3. Pipe outputs never record metadata: `$(title)` in file names is always
   empty, so with an unchanged name every reopen truncates the previous file;
   ID3v2 tags and FLAC comments are always empty
   ([file-callers 1](file-callers.md), [native-codecs 9](native-codecs.md)).
4. A `%theora` link that ends with no image fails the output
   ([ogg D3](ogg.md)).
5. Opus pre-skip is fixed at 3840: 73.5 ms lost at the start of every link
   ([ogg D1](ogg.md)).
6. `%external`: a process exiting early loses its buffered output and fails
   the clock; `restart_on_crash` dies on EPIPE; `restart_on_metadata` drops
   about 80 ms per restart ([simple-encoders 1, 2, 4](simple-encoders.md)).
7. Every native encoder loses its tail at stop: `%fdkaac` its whole delay
   (5672 samples HE-AAC v2), `%mp3` its last buffer, `%shine` its remainder and
   delay line, Opus/Speex/Theora their last partial unit, every resampler its
   held samples ([native-codecs 1–4](native-codecs.md), [ogg D2](ogg.md)).
8. Icecast drops the encoder's final bytes on every disconnect, and the first
   piece of every connection ([net-callers D5, D3](net-callers.md)).
9. libFLAC setup failure: a 0-byte file, exit 0, nothing logged
   ([native-codecs 7](native-codecs.md)).
10. `%ffmpeg` to a plain path with `truncate=0` leaves a stale tail; a
    protocol option kills Ogg chaining at the first metadata change
    ([ffmpeg F4, F2](ffmpeg.md)).

Files that are wrong or unreadable:

11. AV Ogg files start with a skeleton-only link ffprobe cannot read
    ([ogg D9](ogg.md)).
12. `%avi` puts several images per chunk and empty chunks, and Liquidsoap
    rejects its own files ([simple-encoders 5–7](simple-encoders.md)).
13. `%flac` STREAMINFO keeps 0 samples and a zero MD5, and its header is not
    in `header()` ([native-codecs 5, 6](native-codecs.md)).
14. `%wav(duration)` declares a length nothing enforces
    ([simple-encoders 8](simple-encoders.md)).

The rest (configuration checks that disagree between typing and building,
dead options, printing, messages) is in the files, ranked there and in the
verification files.

## Open pull requests touching these findings

The snapshot is `origin/main` 02fa52985. Open PRs on the stack #5703 address
some entries: #5701 (Opus pre-skip), #5706 (`%mp3`/`%shine` Info frame and
tails; its approach is on hold pending this specification).
