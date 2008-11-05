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

(** Output a stream encoded by an external process to an icecast server *)

let no_mount = "Use [name]"
let no_name = "Use [mount]"

let proto =
  Icecast2.proto @ External_encoded.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "bitrate", Lang.int_t, Some (Lang.int 128), None;
    "icy_metadata", Lang.bool_t, Some (Lang.bool true),
    Some "Send new metadata using the ICY protocol.";
    "shout_raw", Lang.bool_t, Some (Lang.bool false), 
    Some "Send to icecast as raw data. No format-specific parsing is done.";
    "protocol", Lang.string_t, (Some (Lang.string "http")),
    Some "Protocol of the streaming server: \
          'http' for Icecast, 'icy' for Shoutcast." ;
    "format", Lang.string_t, Some (Lang.string "mp3"), Some "Shout format. \
                                                  One of \"mp3\" or \"ogg\".";
    "", Lang.source_t, None, None ]

class to_shout p =
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let restart_encoder = e Lang.to_bool "restart_encoder" in
  let restart_on_crash = e Lang.to_bool "restart_on_crash" in
  let header = e Lang.to_bool "header" in
  let raw = e Lang.to_bool "shout_raw" in
  let icy = e Lang.to_bool "icy_metadata" in
  let process = List.assoc "process" p in
  let bitrate = e Lang.to_int "bitrate" in

  let source = List.assoc "" p in
  let mount = s "mount" in
  let name = s "name" in
  let format = 
    match s "format" with
      | "mp3" -> Shout.Format_mp3
      | "ogg" -> Shout.Format_vorbis
      | _ ->
        raise (Lang.Invalid_value
                 ((List.assoc "format" p),
                  "Format must be one of \"mp3\", \"ogg\"."))
  in
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
    [External_encoded.external_encoder] Icecast2.output ~format ~protocol
      ~bitrate:(string_of_int bitrate) ~raw ~mount ~name ~source p as super
  inherit External_encoded.base ~restart_encoder ~restart_on_crash ~header process as base

  method reset_encoder m =
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
      (* Mp3 format just updates meta via ICY.
         Apparently ICY updates also work for
         ogg/vorbis streams.. *)
      if icy then
        begin
          match connection with
            | Some c ->
                (try Shout.set_metadata c a with _ -> ())
            (* Do nothing if shout connection isn't available *)
            | None -> ()
        end;
      if restart_encoder then
        base#reset_encoder m
      else
        ""

    method output_start =
      base#external_output_start External_encoded.initial_meta;
      super#output_start

    method output_stop =
      base#external_output_stop;
      super#output_stop

    method output_reset = self#output_stop; self#output_start
end

let () =
    Lang.add_operator "output.icecast.external" ~category:Lang.Output
      ~descr:
  "Output the source's stream to an icecast2 compatible server using an external encoder."
      proto
      (fun p -> ((new to_shout p):>Source.source))

