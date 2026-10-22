# Language notes: FFmpeg decoder

Advice for building the decoder of [decoder.md](../decoder.md) and its
per-stream files in OCaml, on the FFmpeg bindings. Nothing here is needed to
follow Part A.

## 1. Library layout

- One optional library for the decoder, depending on the core, the FFmpeg
  base library of the layer and the `ffmpeg-av`, `ffmpeg-avcodec`,
  `ffmpeg-avutil`, `ffmpeg-swresample`, `ffmpeg-swscale` and `ffmpeg-avfilter`
  bindings.
- Small modules, each with an `.mli`: settings and lists; the shared helpers
  of decoder.md §16; one module per per-stream family; the decoder record
  (mapping, loop, seek); the image decoder; registrations.
- Parsing `ffmpeg_options` (decoder.md §4.1) reuses the language's
  encoder-parameter parser, so the syntax cannot drift from `%ffmpeg(...)`.

## 2. Per-stream decoders

- A per-stream decoder is a record of closures over one input type:

  ```ocaml
  type 'item t = {
    push : 'item -> unit;
    switch : stream_idx:int64 -> unit;
    flush : unit -> unit;
  }
  type 'item sparse = { decoder : 'item t; advance : int -> unit }
  ```

  The contract clauses map onto the fields; a decoder with nothing to do on
  `switch` gives `ignore`.

- `Av.stream` is typed by media and by read mode
  (``[`Frame]`` or ``[`Packet]``). Pair each stream with its decoder in one variant
  constructor per (media, mode), so dispatch on a
  `read_input` result is a total match.
- The internal audio decoder can pick a first-class module by PCM kind over
  `Swresample.Make (Swresample.Frame)` applied to the three output kinds.
  `Swresample.flush` gives the drain of decoder-internal.md §6.

## 3. Decoder record

- Keep the mapping, the target, the pending queue and the positions in one
  record behind one `Mutex.t`; `decode`, `seek`, `eof` and `close` each take
  it for their whole run. Build the new mapping of a seek inside the lock:
  decoder.md §8.6 is then a property of the code shape, not of timing.
- Model the mapping phase as a variant that carries its data:
  ``[ `Waiting of pending | `Started ]``, and the per-stream threshold as an
  `int option` cleared on first release.
- The pending queue is one `Queue.t` per stream plus a merge on release key;
  no global sort is needed.
- The byte position for the stream decoder's `SEEK_CUR` (decoder.md §5.1) is
  an `int ref` updated by the read and seek closures given to
  `Av.open_input_stream`.

## 4. Bindings gaps

- The container origin of decoder.md §8.3 needs FFmpeg's input start time
  (`AVFormatContext.start_time`), and the discontinuity rule of decoder.md
  §9.2 needs the input format's `AVFMT_TS_DISCONT` flag. Neither is in the
  bindings spec; both are small getters to add there.

## 5. Formats seen in existing data

- `ffmpeg_options` metadata: the argument list of `%ffmpeg(...)` without the
  parentheses.
- `application/ffmpeg;<args>` MIME type with the same argument syntax after
  the first `;`.
- Timed ID3 payloads with and without their `ID3` header; the missing header
  is rebuilt with version bytes `03 00` then `04 00`.
