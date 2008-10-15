(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(** Decode and read metadatas of mp3 files. *)

open Dtools

module Generator = Float_pcm.Generator

let log = Log.make ["format";"mp3"]

let conf_mad =
  Conf.void ~p:(Configure.conf#plug "mad")
    "Mad (mp3) decoding options."
let conf_mime =
  Conf.bool ~p:(conf_mad#plug "check_mime") ~d:true
      "Enable magic mime test, if compiled."
let conf_mime_types =
  Conf.list ~p:(conf_mad#plug "mime_types")
    "Mime-types used for guessing formats"
    ~d:["audio/mpeg";"application/octet-stream";"video/x-unknown"]

let decoder file =
  begin
    if conf_mime#get then
      match Configure.file_mime file with
        (* libmagic does not always detect mp3 as audio/mpeg
         * (Repeat after me: mp3 formats sucks, mp3 format.... )
         * So it might return other types.
         * We hope other formats like jpeg, gif, etc.. 
         * will most likely be correctly detected *)
        | Some s when List.mem s conf_mime_types#get -> ()
        | None -> ()
        | Some s ->
          log#f 2 "Mime type for %s is not valid: %s" file s;
          assert false
  end ;
  let fd =
    log#f 4 "open %S" file ;
    Mad.openfile file
  in
  let abg = Generator.create () in
  let buffer_length = Decoder.buffer_length () in
  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    log#f 4 "close %S" file ;
    Mad.close fd
  in
  let fill =
    fun buf ->
      assert (not !closed) ;

      begin
        try
          while Generator.length abg < buffer_length do
            let data = Mad.decode_frame_float fd in
            let sample_freq,_,_ = Mad.get_output_format fd in
              Generator.feed abg ~sample_freq data
          done
        with
          | _ -> ()
      end ;

      let offset = AFrame.position buf in
        AFrame.fill_frame abg buf ;
        in_bytes := Mad.get_current_position fd ;
        out_samples := !out_samples + AFrame.position buf - offset ;
        (* Compute an estimated number of remaining ticks. *)
        let abglen = Generator.length abg in
          if !in_bytes = 0 then
            0
          else
            let compression =
              (float (!out_samples+abglen)) /. (float !in_bytes)
            in
            let remaining_samples =
              (float (file_size - !in_bytes)) *. compression
              +. (float abglen)
            in
              (* I suspect that in_bytes in not accurate, since I don't
               * get an exact countdown after that in_size=in_bytes.
               * Instead, there is a stall at the beginning
               * after which the countdown starts. *)
              Fmt.ticks_of_samples (int_of_float remaining_samples)
  in
    { Decoder.fill = fill ; Decoder.close = close }

let duration file =
  let ans = Mad.duration file in
  match ans with
    | 0. -> raise Not_found
    | _ -> ans

let () = Decoder.formats#register "MP3"
           (fun name -> try Some (decoder name) with _ -> None);
         Request.dresolvers#register "MP3" duration
