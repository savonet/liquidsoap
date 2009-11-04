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

(** Decode and read ogg files. *)

let log = Dtools.Log.make ["format";"ogg"]

let decoder =
  let converter = Rutils.create () in
  {
   File_decoder.Float.
    log = log;
    openfile =
      (fun file ->
        let sync,fd = Ogg.Sync.create_from_file file in
        begin
         try
          Ogg_demuxer.init sync,fd
         with
           | e -> (try Unix.close fd with _ -> ()); raise e
        end,
        Generator.create ());
    decode =
      (* TODO: also decode video *)
      (fun (decoder,_) abg ->
          let feed ((buf,sample_freq),_) =
            let audio_src_rate = float sample_freq in
            let content,length =
              converter ~audio_src_rate
                    { Frame.
                        audio = buf;
                        video = [||];
                        midi = [||] }
            in
            Generator.feed abg content 0 length
          in
          Ogg_demuxer.decode_audio decoder feed; 
          if Ogg_demuxer.eos decoder then
            raise Ogg_demuxer.End_of_stream);
    position = (fun (_,fd) -> Unix.lseek fd 0 Unix.SEEK_CUR);
    close = (fun (_,fd) -> 
               try                                                                                
                 Unix.close fd                                                                    
               with _ -> ()) 
  }

exception Channels of int

(** Returns the type of an ogg file. *)
(* TODO: add video *)
let get_type file =
  let sync,fd = Ogg.Sync.create_from_file file in
  let close () =
    try
      Unix.close fd
    with _ -> ()
  in
  try
    let decoder = Ogg_demuxer.init sync in
    (* Feed may be called several times *)
    let feed ((buf,_),_) =
      raise (Channels (Array.length buf))
    in
    let audio =
      try
        Ogg_demuxer.decode_audio decoder feed;
        raise Not_found
      with
        | Channels x -> x
    in
    close ();
    { Frame.
        audio = audio ;
        video = 0 ;
        midi  = 0 }
   with
     | e -> close (); raise e

let () = Decoder.formats#register "OGG"
           (fun name kind ->
              let ogg_type = get_type name in
                if Frame.type_has_kind ogg_type kind then
                  try
                    Some (File_decoder.Float.decode decoder name)
                  with _ -> None
                else
                  None)

exception Metadata of (string*string) list

let get_tags ~format file =
  (* Fail if file is not decoded using the OGG demuxer. *)
  if format <> "OGG" then
    raise Not_found;
  let sync,fd = Ogg.Sync.create_from_file file in
  let close () =
    try
      Unix.close fd
    with
      | _ -> ()
  in
  try
    let decoder = Ogg_demuxer.init sync in
    let feed (_,m) =
      let m =
        match m with
          | Some m ->
             Hashtbl.fold
              (fun x -> fun y -> fun z -> (x,y)::z)
              m []
          | None -> []
      in
      raise (Metadata m)
    in
    let m =
      try
        if Ogg_demuxer.has_track Ogg_demuxer.Audio_track decoder then
          Ogg_demuxer.decode_audio decoder feed;
        []
      with
        | Metadata m -> m
    in
    let m =
      try
        if Ogg_demuxer.has_track Ogg_demuxer.Video_track decoder then
          Ogg_demuxer.decode_video decoder feed;
        m
      with
        | Metadata m' -> m@m'
    in
    close ();
    m
  with
    | _ -> close (); []

let () = Request.mresolvers#register "OGG" get_tags
