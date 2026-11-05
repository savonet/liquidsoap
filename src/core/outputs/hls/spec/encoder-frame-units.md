# Encoders that split at every chunk

Part A. `%wav`, `%avi`, `%flac`, `%ogg`, `%external` and `%ndi` implement
the seam of [encoder-contract.md](encoder-contract.md) with one shared
adapter around their plain encoding function. Sections follow the
contract's numbering.

## 1. Role

The adapter declares a split point at the start of every chunk. That is
correct only when each chunk's bytes decode after the header bytes alone,
which these formats approximate by repeating their header at every
segment (§8).

## 2. Calls and order

As the contract. The adapter counts media time as the sum of the chunk
lengths it has encoded.

## 3. `init`

ID3 mode none. `id3 = true` is rejected
([encoder-contract.md §3](encoder-contract.md#3-init)).

## 4. `init_encode`

The first call returns `Init (none, [Bytes (encode chunk)])`.

## 5. `split_encode`

Returns `[Split {time = media time before this chunk; forced = force};
Bytes (encode chunk)]`. Every split is at a chunk start, so `force` never
changes where it is.

## 6. Properties

`codec_attrs`, `bitrate` and `video_size` are none; `independent_segments`
is false; `free_split` is true.

## 7. `insert_id3`

Never called (mode none).

## 8. Header and stop

| Encoder     | `header()`                                                                                            |
| ----------- | ----------------------------------------------------------------------------------------------------- |
| `%wav`      | the WAV header, once the first chunk is encoded; before, empty: the first chunk's bytes start with it |
| `%avi`      | the AVI header, once the first chunk is encoded; before, empty: the first chunk's bytes start with it |
| `%ogg`      | the current Ogg header pages                                                                          |
| `%flac`     | empty                                                                                                 |
| `%external` | empty                                                                                                 |
| `%ndi`      | rejected at construction: NDI only works with `output.ndi`                                            |

`stop()` is the encoder's own.

None of these containers is a segment format RFC 8216 names; each gets the
warning of [variants.md §2](variants.md#2-building-a-variant).

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
