(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Outputs using the LAME encoder for MP3. *)

open Source
open Dtools
open Lame

let create_encoder ~samplerate ~bitrate ~quality ~stereo =
  let enc = Lame.create_encoder () in
    (* Input settings *)
    Lame.set_in_samplerate enc 44100 ;
    Lame.set_num_channels enc 2 ;
    (* Output settings *)
    Lame.set_mode    enc (if stereo then Lame.Stereo else Lame.Mono) ;
    Lame.set_quality enc quality ;
    Lame.set_out_samplerate enc samplerate ;
    Lame.set_brate enc bitrate ;
    Lame.init_params enc ;
    enc

(** Output in an MP3 file *)

class to_file
  ~filename ~samplerate ~bitrate ~quality ~stereo ~autostart source =
object (self)
  inherit
    [Lame.encoder] Output.encoded
         ~name:filename ~kind:"output.file.mp3" ~autostart source

  method reset_encoder encoder m = ""
  method encode e b start len =
    Lame.encode_buffer_float_part e b.(0) b.(1) start len

  val mutable fd = None

  method output_start =
    assert (fd = None) ;
    let enc = create_encoder ~quality ~bitrate ~stereo ~samplerate in
      fd <- Some (open_out filename) ;
      encoder <- Some enc

  method output_stop =
    match fd with
      | None -> assert false
      | Some v -> close_out v ; fd <- None

  method send b =
    match fd with
      | None -> assert false
      | Some fd -> output_string fd b

  method output_reset = ()
end

let () =
  Lang.add_operator "output.file.mp3"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

      "samplerate",
      Lang.int_t,
      Some (Lang.int 44100),
      None ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      None ;

      "quality",
      Lang.int_t,
      Some (Lang.int 5),
      None ;

      "stereo",
      Lang.bool_t,
      Some (Lang.bool true),
      None;

      "",
      Lang.string_t,
      None,
      Some "Filename where to output the MP3 stream." ;

      "", Lang.source_t, None, None ]
    ~category:Lang.Output
    ~descr:"Output the source's stream as an MP3 file."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let quality = e Lang.to_int "quality" in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let samplerate = e Lang.to_int "samplerate" in
       let bitrate = e Lang.to_int "bitrate" in
       let filename = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
         ((new to_file ~filename
             ~quality ~bitrate ~samplerate ~stereo ~autostart source):>source))

(** Output a MP3 stream to an icecast server *)

let no_mount = "Use [name]"
let no_name = "Use [mount]"

let proto =
  Icecast2.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;

    "samplerate", Lang.int_t, Some (Lang.int 44100), None;
    "bitrate", Lang.int_t, Some (Lang.int 128), None;
    "quality", Lang.int_t, Some (Lang.int 5), None;
    "stereo", Lang.bool_t, Some (Lang.bool true), None;
    "protocol", Lang.string_t, (Some (Lang.string "http")),
    Some ("http|icy: protocol of the output server "^
          "('http' for icecast / 'icy' for shoutcast)") ;

    "", Lang.source_t, None, None ]

let no_multicast = "no_multicast"

class to_shout p =
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let stereo = e Lang.to_bool "stereo" in
  let samplerate = e Lang.to_int "samplerate" in
  let bitrate = e Lang.to_int "bitrate" in
  let quality = e Lang.to_int "quality" in

  let source = List.assoc "" p in

  let mount = s "mount" in
  let name = s "name" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 ((List.assoc "mount" p),
                  "Either name or mount must be defined."))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then name else mount
  in
  let protocol =
    let v = List.assoc "protocol" p in
      match Lang.to_string v with
        | "http" -> Shout.Protocol_http
        | "icy" -> Shout.Protocol_icy
        | _ ->
            raise (Lang.Invalid_value
                     (v, "valid values are 'http' (icecast) "^
                      "and 'icy' (shoutcast)"))
  in
object (self)
  inherit
    [Lame.encoder] Icecast2.output ~format:Shout.Format_mp3 ~protocol
      ~mount ~name ~source p as super

  method reset_encoder encoder m =
    let get h k l =
      try
        (k,(Hashtbl.find h k))::l
      with _ -> l
    in
    let getd h k d l =
      try
        (k,(Hashtbl.find h k))::l
      with _ -> (k,d)::l
    in
    let def_title =
      match get m "uri" [] with
        | (_,s)::_ -> let title = Filename.basename s in
            ( try
                String.sub title 0 (String.rindex title '.')
              with
                | Not_found -> title )
        | [] -> "Unknown"
    in
    let song =
      try
        Hashtbl.find m "song"
      with _ ->
        (try
          (Hashtbl.find m "artist") ^ " - "
        with _ -> "")
        ^
        (try
          Hashtbl.find m "title"
        with _ -> "Unknown")
    in
    let a = Array.of_list
      (getd m "title" def_title
         (get m "artist"
            (get m "genre"
               (get m "date"
                  (get m "album"
                     (get m "tracknum"
                        (get m "comment"
                          (getd m "song" song [])))))))) (* for Shoutcast *)
    in
      match connection with
        | Some c ->
            Shout.set_metadata c a ; ""
        | None -> assert false

  method encode e b start len =
    Lame.encode_buffer_float_part e b.(0) b.(1) start len

  method output_start =
    super#output_start ;
    log 3 "Setting up an MP3 encoder..." ;
    let enc = create_encoder ~bitrate ~samplerate ~quality ~stereo in
      encoder <- Some enc

end

let () =
    Lang.add_operator "output.icecast.mp3" ~category:Lang.Output
      ~descr:
  "Output the source's stream to an icecast2 compatible server in MP3 format."
      proto
      (fun p -> ((new to_shout p):>Source.source))
