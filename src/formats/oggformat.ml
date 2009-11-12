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

module Generator = Generator.From_audio_video

exception Channels of int

let converter = ref None
let converter () =
  match !converter with
    | None ->
      let conv =
        Video_converter.find_converter
          (Video_converter.YUV Video_converter.Yuvj_420)
          (Video_converter.RGB Video_converter.Rgba_32)
      in
      converter := Some conv;
      conv
    | Some conv -> conv

(** Convert a frame *)
let video_convert buf = 
  let converter = converter () in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let rgb = RGB.create width height in
  let frame = Video_converter.frame_of_internal_rgb rgb in
  converter
    (Video_converter.frame_of_internal_yuv
       buf.Ogg_demuxer.y_width buf.Ogg_demuxer.y_height
      ((buf.Ogg_demuxer.y, buf.Ogg_demuxer.y_stride),
          (buf.Ogg_demuxer.u,
           buf.Ogg_demuxer.v,
           buf.Ogg_demuxer.uv_stride)))
    frame;
  rgb

let decoder =
  {
   File_decoder.
    log = log;
    openfile =
      (fun file ->
        let sync,fd = Ogg.Sync.create_from_file file in
        begin
         try
           Ogg_demuxer.init sync,
           fd,
           Rutils.create_audio ()
         with
           | e -> (try Unix.close fd with _ -> ()); raise e
        end);
    get_kind = 
      (fun (decoder,_,_) ->
        let feed ((buf,_),_) =
          raise (Channels (Array.length buf))
        in
        let audio =
          try
            Ogg_demuxer.decode_audio decoder feed;
            raise Not_found
          with
            | Channels x -> x
            | Not_found  -> 0
        in
        let feed (buf,_) =                                                                  
          raise (Channels 1)                                                                
        in                                                                                  
        let video =                                                                         
          try                                                                               
            Ogg_demuxer.decode_video decoder feed;                                          
            raise Not_found                                                                 
          with                                                                              
            | Channels x -> x                                                               
            | Not_found  -> 0                                                               
        in    
        { Frame.
            audio = Frame.mul_of_int audio ;
            video = Frame.mul_of_int video ;
            midi  = Frame.mul_of_int 0 });
    decode =
      (fun (decoder,_,converter) buffer ->
          if Ogg_demuxer.eos decoder then
            raise Ogg_demuxer.End_of_stream;
          let feed ((buf,sample_freq),_) =
            let audio_src_rate = float sample_freq in
            let content,length =
              converter ~audio_src_rate buf
            in
            Generator.put_audio buffer content 0 length
          in
          begin
           try
            Ogg_demuxer.decode_audio decoder feed
           with Not_found -> ()
          end ;
          let feed (buf,_) =               
            (* TODO: rate conversion !                                               
            let src_rate = buf.Ogg_demuxer.fps in *)
            assert( 
              int_of_float (buf.Ogg_demuxer.fps +. 0.5)
               = Frame.video_of_seconds 1.) ;
            let length = Frame.master_of_video 1 in
            let frame = video_convert buf in
            Generator.put_video buffer [|[|frame|]|] 0 length
          in
          (* Try to decode video. *)
          begin
            try
              Ogg_demuxer.decode_video decoder feed
            with
              | Not_found -> ()
          end );
    position = (fun (_,fd,_) -> Unix.lseek fd 0 Unix.SEEK_CUR);
    close = (fun (_,fd,_) -> 
               try                                                                                
                 Unix.close fd                                                                    
               with _ -> ()) 
  }

let () = Decoder.formats#register "OGG" (File_decoder.decode decoder)

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
