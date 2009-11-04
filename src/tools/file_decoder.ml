(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** Generic file decoder. *)

module type Generator_t =
sig
  type t
  val length : t -> int
  val clear : t -> unit
  val fill : t -> Frame.t -> unit
end

module Decoder(Generator:Generator_t) = 
struct

  type 'a file_decoder = 
    {
      log      : Dtools.Log.t;
      openfile : string -> 'a*Generator.t;
      get_type : 'a -> Frame.content_type;
      decode   : 'a -> Generator.t -> unit; 
      position : 'a -> int;
      close    : 'a -> unit
    }

  let decode decoder file = 
    let fd,abg =
      decoder.log#f 5 "open %S" file ;
      try
        decoder.openfile file
      with
        | e -> 
           decoder.log#f 5 "Could not decode file." ;
           raise e
    in
    let buffer_length =
      Frame.audio_of_seconds Decoder.conf_buffer_length#get
    in
    let stats = Unix.stat file in
    let file_size = stats.Unix.st_size in
    let out_samples = ref 0 in
    let closed = ref false in
    let close () =
      assert (not !closed) ;
      closed := true ;
      decoder.log#f 5 "close %S" file ;
      decoder.close fd
    in
   let fill =
      fun buf ->
        assert (not !closed) ;
  
        begin
          try
            while Generator.length abg < buffer_length do
              decoder.decode fd abg
            done
          with
            | _ -> () 
        end ;
  
        let offset = AFrame.position buf in
          Generator.fill abg buf ;
          let in_bytes = decoder.position fd in
          out_samples := !out_samples + AFrame.position buf - offset ;
          (* Compute an estimated number of remaining ticks. *)
          let abglen = Generator.length abg in
            if in_bytes = 0 then
              0
            else
              let compression =
                (float (!out_samples+abglen)) /. (float in_bytes)
              in
              let remaining_samples =
                (float (file_size - in_bytes)) *. compression
                +. (float abglen)
              in
                (* I suspect that in_bytes in not accurate, since I don't
                 * get an exact countdown after that in_size=in_bytes.
                 * Instead, there is a stall at the beginning
                 * after which the countdown starts. *)
                Frame.master_of_audio (int_of_float remaining_samples)
    in
      { Decoder.fill = fill ; Decoder.close = close }

   (** Top-level wrapper to also check content_type *)
   let decode decoder name kind = 
     try
       let data_type = 
         let (data,_) = decoder.openfile name in
         let data_type = 
          try
            decoder.get_type data
          with
           | e -> decoder.close data; raise e
         in
         decoder.close data; 
         data_type
       in
       if Frame.type_has_kind data_type kind then
         Some (decode decoder name)
       else
         None
      with _ -> None
end

module Float = Decoder(Generator)
