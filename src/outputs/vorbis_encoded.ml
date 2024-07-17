(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
]

class virtual base freq stereo =
  let samples_per_second = float (Fmt.samples_per_second()) in
object (self)
  val tmp =
    if stereo then
      [||]
    else
      Float_pcm.create_buffer 1 (Fmt.samples_per_frame())

  val mutable os = Ogg.Stream.create ()

  method virtual new_encoder : Vorbis.Encoder.t

  (** Do we have to flush before paging out the next data? *)
  val mutable must_flush = false

  method reset_encoder encoder m =
    let get h k =
      try
        Some (Hashtbl.find h k)
      with _ -> None
    in
    let getd h k d =
      try
        Some (Hashtbl.find h k)
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
      Vorbis.Encoder.end_of_stream encoder os;
      let flushed = Ogg.Stream.flush os in
        os <- Ogg.Stream.create ();
        (* Vorbis.Encoder.reset encoder; *)
        Vorbis.Encoder.headerout self#new_encoder os
          (Vorbis.tags
             ?title:(getd m "title" def_title)
             ?artist:(get m "artist")
             ?genre:(get m "genre")
             ?date:(get m "date")
             ?album:(get m "album")
             ?tracknumber:(get m "tracknum")
             ?comment:(get m "comment")
             ());
        (* The header must be on its own page. *)
        must_flush <- true;
        flushed

  method encode e b start len =
    let b =
      if stereo then b else begin
        for i = start to start+len-1 do
          tmp.(0).(i) <- (b.(0).(i) +. b.(1).(i)) /. 2.
        done ;
        tmp
      end
    in
    let buf =
      Float_pcm.resample
        (float freq /. samples_per_second)
        b start len
    in
    let f =
      if must_flush then
        (
          must_flush <- false;
          Ogg.Stream.flush os
        )
      else
        ""
    in
      Vorbis.Encoder.encode_buffer_float e os buf 0 (Array.length buf.(0));
      f ^ Ogg.Stream.pagesout os
end

(** Output in an Ogg/vorbis file. *)
class to_file
  filename ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~quality ~mode ~bitrate freq stereo source autostart =
object (self)
  inherit
    [Vorbis.Encoder.t] Output.encoded
      ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit base freq stereo as base

  method new_encoder =
    let enc =
      (* TODO: log message when the creation of the encoder fails *)
      let nom,min,max = bitrate in
      match mode with
        | ABR
        | CBR -> Vorbis.Encoder.create (Fmt.channels ()) freq max nom min
        | VBR -> Vorbis.Encoder.create_vbr (Fmt.channels ()) freq quality
    in
      encoder <- Some enc ;
      enc

  method reset_encoder enc m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    (* Avoid multiple resets *)
    if must_flush then "" else base#reset_encoder enc m

  method output_start =
    to_file#file_output_start ;
    let enc = self#new_encoder in
    let tags =
      Vorbis.tags ~artist:"The Savonet Team" ~title:"Liquidsoap Stream" ()
    in
      Vorbis.Encoder.headerout enc os tags ;
      must_flush <- true

  method output_stop =
    Vorbis.Encoder.end_of_stream (Utils.get_some encoder) os ;
    to_file#send (Ogg.Stream.flush os) ;
    os <- Ogg.Stream.create () ;
    to_file#file_output_stop

end

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
         ((new to_file
             name ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~quality:0. ~mode:ABR ~bitrate:(bitrate, min_bitrate, max_bitrate)
             freq stereo source autostart):>source))

let () =
  Lang.add_operator "output.file.vorbis.cbr" (* Constant BitRate *)
    (vorbis_proto @ File_output.proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      Some "Bitrate (in kbps).";

      "", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis file "
            ^ "in Constant BitRate mode.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
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
       let source = Lang.assoc "" 2 p in
         ((new to_file
             name ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~quality:0. ~mode:CBR ~bitrate:(bitrate, bitrate, bitrate)
             freq stereo source autostart):>source))

let () =
  Lang.add_operator "output.file.vorbis" (* Variable BitRate *)
    (vorbis_proto @ File_output.proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "quality",
      Lang.float_t,
      Some (Lang.float 2.),
      Some ("Desired quality level, currently from -1. to 10. (low to high).");

      "", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis file "
            ^ "in Variable BitRate mode.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
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
         ((new to_file
             name ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~quality ~mode:VBR ~bitrate:(0,0,0)
             freq stereo source autostart):>source))

