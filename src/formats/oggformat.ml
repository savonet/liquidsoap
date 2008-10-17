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

(** Decode and read metadatas of ogg files. *)

module Generator = Float_pcm.Generator

let log = Dtools.Log.make ["ogg.demuxer"]

let decoder file =
  let sync,fd = Ogg.Sync.create_from_file file in
  let decoder = Ogg_demuxer.init sync in
  let init_meta = ref true in
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
    try
      Unix.close fd
    with _ -> ()
  in
  let fill buf =
    assert (not !closed) ;

    (* Video input *)
    (* TODO: video buffer with conversion.. *)
    if Fmt.video_channels () <> 0 &&
       Ogg_demuxer.has_track Ogg_demuxer.Video_track decoder then
    begin
      let b = VFrame.get_rgb buf in
      let feed b c i =
        let feed (buf,_) =
          (* TODO: video conversion *)
          if
            buf.Ogg_demuxer.uv_width <> buf.Ogg_demuxer.y_width / 2 ||
            buf.Ogg_demuxer.uv_height <> buf.Ogg_demuxer.y_height / 2 ||
            int_of_float (buf.Ogg_demuxer.fps +. 0.5) <> Fmt.video_frames_per_second () (* TODO: more precise? + convert fps *)
          then
            assert false;
          (* TODO: choose the kind of scaling *)
          let scale b =
            if buf.Ogg_demuxer.y_width = Fmt.video_width () &&
               buf.Ogg_demuxer.y_height = Fmt.video_height () then
              b
            else
              RGB.proportional_scale_to b (Fmt.video_width ()) (Fmt.video_height ())
          in
            b.(c).(i) <- scale (RGB.of_YUV420 (buf.Ogg_demuxer.y, buf.Ogg_demuxer.u, buf.Ogg_demuxer.v) buf.Ogg_demuxer.y_width);
        in
        feed
      in
      let off = VFrame.position buf in
      let size = VFrame.size buf in
        for c = 0 to Array.length b - 1 do
            for i = off to size - 1 do
              Ogg_demuxer.decode_video decoder (feed b c i);
            done;
        done;
      (* TODO: sort out when and who puts a break: 
       * this end break is already added by
       * audio filling.. *)
      (*VFrame.add_break buf size;*)
    end;

    begin
      try
        while Generator.length abg < buffer_length do
          let feed ((buf,sample_freq),meta) = 
            begin
              match meta with
                | Some meta ->
                    (* Initial meta is read by 
                     * the resolver.. *)
                    if !init_meta then
                      init_meta := false
                    else
                      Generator.add_metadata abg (0,meta)
                | _ -> ()
            end;
            Generator.feed abg ~sample_freq buf
          in
          try
            Ogg_demuxer.decode_audio decoder feed;
            if !(decoder.Ogg_demuxer.eos) then
              begin
                Ogg_demuxer.reset decoder
              end
          with
            | Ogg_demuxer.End_of_stream -> ()
        done;
      with
        | e -> log#f 4 "ogg file decoder exited on exception: %s" 
                          (Printexc.to_string e)
    end ;

    let offset = AFrame.position buf in
    AFrame.fill_frame abg buf;
    in_bytes := Unix.lseek fd 0 Unix.SEEK_CUR ;
    out_samples := !out_samples + AFrame.position buf - offset ;
    (* Compute an estimated number of remaining ticks. *)
    assert (!in_bytes!=0) ;
    let compression =
      (float (!out_samples)) /. (float !in_bytes)
    in
    let remaining_samples =
      (float (file_size - !in_bytes)) *. compression
    in
    (* I suspect that in_bytes in not accurate, since I don't
     * get an exact countdown after than in_size=in_bytes, but there
     * is a stall at the beginning after which the countdown starts. *)
    Fmt.ticks_of_samples (int_of_float remaining_samples)
  in
    { Decoder.fill = fill ; Decoder.close = close }

let () =
  Decoder.formats#register "OGG"
    (fun name -> try Some (decoder name) with _ -> None)

exception Metadata of (string*string) list

let get_tags ~format file =
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
        Ogg_demuxer.decode_audio decoder feed;
        []
      with
        | Metadata m -> m
    in 
    close ();
    m
  with
    | _ -> close (); []

let () = Request.mresolvers#register "OGG" get_tags

