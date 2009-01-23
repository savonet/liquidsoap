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

open Source

type vorbis_encoding_mode = ABR | CBR | VBR

let vorbis_proto = [
  "samplerate",
  Lang.int_t,
  Some (Lang.int 44100),
  None;

  "stereo",
  Lang.bool_t,
  Some (Lang.bool true),
  None;
] @ (Ogg_output.ogg_proto false)

let create ~quality ~mode ~bitrate samplerate stereo =
  let create_encoder ogg_enc m =
    let get h k =
      try
        Some (List.assoc k h)
      with _ -> None
    in
    let getd h k d =
      try
        Some (List.assoc k h)
      with _ -> Some d
    in
    let def_title =
      match get m "uri" with
        | Some s -> let title = Filename.basename s in
            (try
               String.sub title 0 (String.rindex title '.')
             with
               | Not_found -> title)
        | None -> "Unknown"
    in
    let metadata = 
       (Vorbis.tags
            ?title:(getd m "title" def_title)
            ?artist:(get m "artist")
            ?genre:(get m "genre")
            ?date:(get m "date")
            ?album:(get m "album")
            ?tracknumber:(get m "tracknum")
            ?comment:(get m "comment")
              ())
    in
    let channels =
      if not stereo then 1 else Fmt.channels () in
    let enc =
      (* TODO: log message when the creation of the encoder fails *)
      let average_rate,min_rate,max_rate = bitrate in
      match mode with
        | ABR -> Vorbis_format.create_abr ~channels ~samplerate 
                                          ~max_rate ~average_rate 
                                          ~min_rate ~metadata ()
        | CBR -> Vorbis_format.create_cbr ~channels ~samplerate 
                                          ~bitrate:average_rate 
                                          ~metadata ()
        | VBR -> Vorbis_format.create ~channels ~samplerate 
                                      ~quality ~metadata ()
    in
      Ogg_encoder.register_track ogg_enc enc
  in
  let src_freq = float (Fmt.samples_per_second ()) in
  let dst_freq = float samplerate in
  let encode = 
    Ogg_output.encode_audio 
      ~stereo ~dst_freq ~src_freq () 
  in
  create_encoder,encode

let () =
  Lang.add_operator "output.file.vorbis.abr" (* Average BitRate *)
    (vorbis_proto @ File_output.proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      Some "Target bitrate (in kbps).";

      "min_bitrate",
      Lang.int_t,
      Some (Lang.int 118),
      Some "Minimum bitrate (in kbps).";

      "max_bitrate",
      Lang.int_t,
      Some (Lang.int 138),
      Some "Maximum bitrate (in kbps).";

      "", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis file "
            ^ "in Average BitRate mode.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let bitrate = (e Lang.to_int "bitrate") * 1000 in
       let min_bitrate = (e Lang.to_int "min_bitrate") * 1000 in
       let max_bitrate = (e Lang.to_int "max_bitrate") * 1000 in
       let freq = e Lang.to_int "samplerate" in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let source = Lang.assoc "" 2 p in
       let streams = 
         ["vorbis",create ~quality:0. ~mode:ABR 
                          ~bitrate:(bitrate, min_bitrate, max_bitrate) 
                          freq stereo]
       in
         ((new Ogg_output.to_file
             name ~append ~perm ~dir_perm ~streams ~skeleton
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~autostart source):>source))

let () =
  Lang.add_operator "output.file.vorbis.cbr" (* Constant BitRate *)
    (vorbis_proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      Some "Bitrate (in kbps)."] @ File_output.proto @
      ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis file "
            ^ "in Constant BitRate mode.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let bitrate = (e Lang.to_int "bitrate") * 1000 in
       let freq = e Lang.to_int "samplerate" in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let streams =
         ["vorbis",create ~quality:0. ~mode:CBR
                          ~bitrate:(bitrate, bitrate, bitrate)
                          freq stereo]
       in
       let source = Lang.assoc "" 2 p in
         ((new Ogg_output.to_file
             name ~append ~perm ~dir_perm ~streams ~skeleton
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~autostart source):>source))

let () =
  Lang.add_operator "output.file.vorbis" (* Variable BitRate *)
    (vorbis_proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "quality",
      Lang.float_t,
      Some (Lang.float 2.),
      Some ("Desired quality level, currently from -1. to 10. (low to high).")] 

      @ File_output.proto @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis file "
            ^ "in Variable BitRate mode.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let quality = (e Lang.to_float "quality") *. 0.1 in
       let freq = e Lang.to_int "samplerate" in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let source = Lang.assoc "" 2 p in
       let streams =
         ["vorbis",create ~quality ~mode:VBR
                          ~bitrate:(0, 0, 0)
                          freq stereo]
       in
         ((new Ogg_output.to_file
             name ~append ~perm ~dir_perm ~streams ~skeleton 
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~autostart source):>source))

