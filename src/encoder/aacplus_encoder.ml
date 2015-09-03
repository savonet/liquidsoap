(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

(** AAC+ encoder *)

open Encoder
open Encoder.AACPlus

module type Aacplus_t =
sig
  exception Invalid_data
  exception Invalid_config
  type t
  val create : channels:int -> samplerate:int -> bitrate:int -> unit -> t
  val frame_size : t -> int
  val encode : t -> float array array -> string
end

module G = Generator.Generator

module Register(Aacplus:Aacplus_t) = 
struct
  let register_encoder name = 
    let create_encoder ~samplerate ~bitrate ~channels =
      let bitrate = bitrate*1000 in
      Aacplus.create ~channels ~samplerate ~bitrate ()
    in
    let encoder aacplus =
      let channels = aacplus.channels in
      let enc = create_encoder ~samplerate:aacplus.samplerate 
                               ~bitrate:aacplus.bitrate 
                               ~channels 
      in
      let samplerate_converter =
        Audio_converter.Samplerate.create channels
      in
      let has_encoded = ref false in
      let samplerate = aacplus.samplerate in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float samplerate in
      (* Aacplus accepts data of a fixed length.. *)
      let samples = Aacplus.frame_size enc in
      let data = Audio.create channels samples in
      let buf = G.create () in
      let encoded = Buffer.create 1024 in
      let encode frame start len =
        let start = Frame.audio_of_master start in
        let b = AFrame.content_of_type ~channels frame start in
        let len = Frame.audio_of_master len in
        let b,start,len =
          if src_freq <> dst_freq then
            let b = Audio_converter.Samplerate.resample
              samplerate_converter (dst_freq /. src_freq)
              b start len
            in
            b,0,Array.length b.(0)
          else
            Audio.copy b,start,len
        in
        G.put buf b start len ;
        while (G.length buf > samples) do
          let l = G.get buf samples in
          let f (b,o,o',l) = 
            Audio.blit b o data o' l
          in
          List.iter f l ;
          has_encoded := true;
          Buffer.add_string encoded (Aacplus.encode enc data)
        done;
        let ret = Buffer.contents encoded in
        Buffer.reset encoded;
        ret
      in
      (* There is a bug in libaacplus when closing 
       * an encoder that has not yet encoded any data.
       * Thus, we force it to happen before closing.. *)
      let stop () = 
        if not !has_encoded then
          ignore(Aacplus.encode enc data) ;
        ""
      in
        {
          insert_metadata = (fun _ -> ()) ;
          header = None ;
          encode = encode ;
          stop = stop
        }
    in
    Encoder.plug#register name
      (function
         | Encoder.AACPlus m -> Some (fun _ _ -> encoder m)
         | _ -> None)
end
