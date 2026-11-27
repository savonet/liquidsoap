(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Mm

(** WAV encoder *)

open Wav_format

let log = Log.make ["encoder"; "wav"]

(* The sizes of a header: the RIFF size at 4, the data size in its last four
   bytes. *)
let rewrite_sizes (sink : Encoder.sink) ~header_length ~payload ~padding =
  match sink.seek with
    | Some seek when payload <= Wav_aiff.max_size - (header_length - 8) ->
        seek 4;
        sink.write
          (Strings.of_string
             (Wav_aiff.int_string (header_length - 8 + payload + padding)));
        seek (header_length - 4);
        sink.write (Strings.of_string (Wav_aiff.int_string payload))
    | _ -> ()

let same_header header existing =
  let without_sizes header = String.sub header 8 (String.length header - 12) in
  String.length existing = String.length header
  && String.sub existing 0 4 = "RIFF"
  && without_sizes existing = without_sizes header

let encoder ~pos wav { Encoder.capabilities; sink; name; _ } =
  let channels = wav.channels in
  let sample_rate = Deferred.force wav.samplerate in
  let sample_size = wav.samplesize in
  let ratio =
    float sample_rate /. float (Lazy.Mutexed.force Frame.audio_rate)
  in
  let converter = Audio_converter.Samplerate.create channels in
  if wav.duration <> None then
    log#important
      "%s: the duration parameter of %%wav is deprecated and ignored" name;
  let header = Wav_aiff.wav_header ~channels ~sample_rate ~sample_size () in
  let header_length = String.length header in
  (* A continuing writer extends the stream already there. *)
  let need_header = ref (wav.header && not capabilities.Encoder.continuing) in
  let wrote_header = !need_header in
  let payload = ref 0 in
  (* Encodes samples at the stream's rate: those of a frame, and at the end
     those the rate converter still holds. *)
  let encode_converted b start len =
    let s = Bytes.create (sample_size / 8 * len * channels) in
    let of_audio =
      match sample_size with
        | 32 -> fun buf s off -> Audio.S32LE.of_audio buf s off
        | 24 -> fun buf s off -> Audio.S24LE.of_audio buf s off
        | 16 -> Audio.S16LE.of_audio
        | 8 -> fun buf s off -> Audio.U8.of_audio buf s off
        | _ -> Lang_encoder.raise_error ~pos "Unsupported sample size"
    in
    of_audio b start s 0 len;
    payload := !payload + Bytes.length s;
    let s = Bytes.unsafe_to_string s in
    if !need_header then (
      need_header := false;
      Strings.of_list [header; s])
    else Strings.of_string s
  in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample converter ratio b 0 len
    in
    encode_converted b start len
  in
  let encode_drained () =
    let tail = Audio_converter.Samplerate.drain converter in
    encode_converted tail 0 (Mm.Audio.length tail)
  in
  (* On a seekable sink the header's sizes cover what the file holds. *)
  let finalise () =
    match (wav.header, sink.Encoder.read, sink.Encoder.size) with
      | true, _, _ when wrote_header ->
          let padding = !payload land 1 in
          if 0 < padding && sink.Encoder.seek <> None then
            sink.write (Strings.of_string "\000");
          rewrite_sizes sink ~header_length ~payload:!payload ~padding
      | true, Some read, Some size when capabilities.Encoder.continuing ->
          if same_header header (read 0 header_length) then
            rewrite_sizes sink ~header_length
              ~payload:(size () - header_length)
              ~padding:0
          else
            log#severe
              "%s: the file does not start with a WAV header of this format, \
               its header is left unchanged"
              name
      | _ -> ()
  in
  ( finalise,
    {
      Encoder.encode_metadata = (fun _ -> ());
      encode;
      header =
        (fun () ->
          if wrote_header && not !need_header then Strings.of_string header
          else Strings.empty);
      stop = encode_drained;
      bitrate = (fun () -> None);
      access_points = None;
      restart_offset = (fun () -> None);
    } )

let () =
  Plug.register Encoder.plug "wav" ~doc:"Native wav encoder." (function
    | Encoder.WAV w ->
        Some
          (fun creation ->
            let before_end, encoder =
              encoder ~pos:creation.Encoder.pos w creation
            in
            Encoder.writer_of_encoder ~before_end encoder creation.Encoder.sink)
    | _ -> None)
