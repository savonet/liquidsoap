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

(** MP3 encoder *)

open Encoder
open Encoder.MP3

module type Lame_t = 
sig
  type encoder
  val create_encoder : unit -> encoder
  val set_in_samplerate : encoder -> int -> unit
  val set_num_channels : encoder -> int -> unit
  val set_out_samplerate : encoder -> int -> unit
  val set_quality : encoder -> int -> unit
  type mode =
    | Stereo (** stereo, channels encoded independely *)
    | Joint_stereo (** stereo, channels encoded together *)
    | Dual_channel (** not supported *)
    | Mono (** mono *)
  val set_mode : encoder -> mode -> unit
  val set_brate : encoder -> int -> unit
  exception Init_params_failed
  val init_params : encoder -> unit
  val init_bitstream : encoder -> unit
  exception Init_params_not_called
  exception Psychoacoustic_problem
  exception Unknown_error of int
  val encode_buffer_float_part :
      encoder -> float array -> float array -> int -> int -> string
end

module Register(Lame : Lame_t) = 
struct
  let register_encoder name =
    let create_encoder ~samplerate ~bitrate ~stereo =
      let enc = Lame.create_encoder () in
      (* Input settings *)
      Lame.set_in_samplerate enc (Lazy.force Frame.audio_rate) ;
      Lame.set_num_channels enc (if stereo then 2 else 1) ;
      (* Output settings *)
      Lame.set_mode enc (if stereo then Lame.Stereo else Lame.Mono);
      begin                  
        match bitrate with
          | Encoder.MP3.Quality quality ->
               Lame.set_quality enc quality
          | Encoder.MP3.Bitrate bitrate ->
               Lame.set_brate enc bitrate
      end;
      Lame.set_out_samplerate enc samplerate ;
      Lame.init_params enc;
      enc
    in
    let mp3_encoder mp3 = 
      let channels = if mp3.stereo then 2 else 1 in
      let enc = create_encoder ~samplerate:mp3.samplerate 
                               ~bitrate:mp3.bitrate 
                               ~stereo:mp3.stereo 
      in
      let encode frame start len =
        let start = Frame.audio_of_master start in
        let b = AFrame.content_of_type ~channels frame start in
        let len = Frame.audio_of_master len in
        if channels = 1 then
          Lame.encode_buffer_float_part enc b.(0) b.(0) start len
        else
          Lame.encode_buffer_float_part enc b.(0) b.(1) start len
      in
        {
          insert_metadata = (fun m -> ()) ;
          encode = encode ;
          header = None ;
          stop = (fun () -> "")
        }
    in
    Encoder.plug#register name
      (function
         | Encoder.MP3 m -> Some (fun _ _ -> mp3_encoder m)
         | _ -> None)
end
