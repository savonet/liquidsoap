(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Decode and read metadata from mp3 files. *)

open Dtools

let log = Log.make ["decoder";"mp3"]

module Make (Generator:Generator.S_Asio) =
struct

let create_decoder input =
  let resampler = Rutils.create_audio () in
  let fd = Mad.openstream input in
    Decoder.Decoder (fun gen ->
      let data = Mad.decode_frame_float fd in
      let sample_freq,channels,_ = Mad.get_output_format fd in
      let content,length =
        resampler ~audio_src_rate:(float sample_freq) data
      in
        Generator.set_mode gen `Audio ;
        Generator.put_audio gen content 0 (Array.length content.(0)))

end

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered(G)
module D = Make(G)

let create_file_decoder filename kind =
  let generator = G.create `Audio in
    Buffered.file_decoder filename kind D.create_decoder generator


(* Get the number of channels of audio in an MP3 file.
 * This is done by decoding a first chunk of data, thus checking
 * that libmad can actually open the file -- which doesn't mean much. *)
let get_type filename =
  let fd = Mad.openfile filename in
    Tutils.finalize ~k:(fun () -> Mad.close fd)
      (fun () ->
         ignore(Mad.decode_frame_float fd);
         let rate,channels,_ = Mad.get_output_format fd in
           log#f 4
             "Libmad recognizes %S as MP3 (%dHz,%d channels)."
             filename rate channels ;
           { Frame.
             audio = channels ;
             video = 0 ;
             midi  = 0 })

let () =
  Decoder.file_decoders#register
  "MP3/libmad"
  ~sdoc:"Use libmad to decode any file \
         if its MIME type or file extension is appropriate."
  (fun ~metadata filename kind ->
     let log = log#f 3 "%s" in
       if not (Decoder.test_mp3 ~log filename) then
         None
       else
         if kind.Frame.audio = Frame.Variable ||
            kind.Frame.audio = Frame.Succ Frame.Variable ||
            (* libmad always respects the first two kinds *)
            Frame.type_has_kind (get_type filename) kind
         then
           Some (fun () -> create_file_decoder filename kind)
         else
           None)

module D_stream = Make(Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register
    "MP3/libmad"
    ~sdoc:"Use libmad to decode any stream with an appropriate MIME type."
     (fun mime kind ->
        let (<:) a b = Frame.mul_sub_mul a b in
          if List.mem mime Decoder.mp3_mime_types#get &&
             (* Check that it is okay to have zero video and midi,
              * and at least one audio channel. *)
             Frame.Zero <: kind.Frame.video &&
             Frame.Zero <: kind.Frame.midi &&
             kind.Frame.audio <> Frame.Zero
          then
            (* In fact we can't be sure that we'll satisfy the content
             * kind, because the MP3 stream might be mono or stereo.
             * For now, we let this problem result in an error at
             * decoding-time. Failing early would only be an advantage
             * if there was possibly another plugin for decoding
             * correctly the stream (e.g. by performing conversions). *)
            Some D_stream.create_decoder
          else
            None)

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) Decoder.mp3_mime_types#get
    | None -> (try ignore (get_type filename) ; true with _ -> false)

let duration file =
  if not (check file) then raise Not_found ;
  let ans = Mad.duration file in
  match ans with
    | 0. -> raise Not_found
    | _ -> ans

let () =
  Request.dresolvers#register "MP3" duration
