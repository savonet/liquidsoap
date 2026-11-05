# Encoders that split at every chunk

Part A. As built. `%wav`, `%avi`, `%flac`, `%ogg`, `%external` and `%ndi`
implement the seam of [encoder-contract.md](encoder-contract.md) with one
shared adapter around their plain encoding function. Sections follow the
contract's numbering.

## 1. Role

The adapter declares a split point at the start of every chunk. Correct
only when each chunk's bytes are decodable after the header bytes alone,
which these formats approximate by repeating their header at every
segment (§8).

## 2. Calls and order

The factory ignores the HLS flag. The adapter counts media time as the sum
of chunk lengths it has encoded.

## 3. `init`

Ignores both arguments and returns `false`: no ID3, also when the variant
asks `id3 = true` (no error).

## 4. `init_encode`

The first call returns `Init (none, [Bytes (encode chunk)])`: no init
segment.

## 5. `split_encode`

Returns `[Split {time = count before this chunk; forced = force};
Bytes (encode chunk)]`. Every chunk start is a split point, so the output's
grid cuts land on the first chunk at or after each boundary, and a forced
split is in practice never needed (a recorded split point always exists
once a segment holds two chunks).

## 6. Properties

`codec_attrs`, `bitrate` and `video_size` are none; `independent_segments`
is false. So bandwidth comes from the format or the variant field; for
these formats the format gives none, and a variant without `bandwidth`
fails at its first segment close
([variants.md §3](variants.md#3-derived-properties)).

## 7. `insert_id3`

Returns none.

## 8. Header and stop

| Encoder     | `header()`                                                                                             | Effect under HLS                                                                               |
| ----------- | ------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------- |
| `%wav`      | the WAV header once the first chunk is encoded (and the format's `header` option is on); before, empty | every segment after the first starts with a WAV header; the first gets it from its first chunk |
| `%avi`      | the AVI header once the first chunk is encoded; before, empty                                          | same                                                                                           |
| `%ogg`      | the current Ogg header pages                                                                           | every later segment starts with the logical streams' header pages                              |
| `%flac`     | empty                                                                                                  | no repeated header                                                                             |
| `%external` | empty                                                                                                  | none                                                                                           |
| `%ndi`      | raises: NDI only works with `output.ndi`                                                               | the first segment open fails ([operator.md §6](operator.md#6-errors-and-reopening))            |

`stop()` is the encoder's own.

`%ogg` restarts its logical streams on in-stream metadata, but the output
never sends it in-stream metadata ([metadata.md §1](metadata.md#1-track-metadata)),
so no restart happens under HLS.

None of these containers is a segment format RFC 8216 names
([findings](findings.md), F-ENC-1).

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
