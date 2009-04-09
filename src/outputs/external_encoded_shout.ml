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

(** Output a stream encoded by an external process to an icecast server *)

let no_mount = "Use [name]"
let no_name = "Use [mount]"

let proto =
  (Icecast2.proto ~no_mount ~no_name) @ External_encoded.proto @
  [ "start", Lang.bool_t, Some (Lang.bool true),
    Some "Start output threads on operator initialization." ;
    "bitrate", Lang.int_t, Some (Lang.int (-1)),
    Some "Bitrate information for icecast. Not used if negative.";
    "quality", Lang.float_t, Some (Lang.float (-1.)),
    Some "Quality information for icecast. Not used if negative.";
    "samplerate", Lang.int_t, Some (Lang.int (-1)),
    Some "Samplerate information for icecast. Not used if negative.";
    "channels", Lang.int_t, Some (Lang.int (-1)),
    Some "Channels information for icecast. Not used if negative.";
    "icy_metadata", Lang.bool_t, Some (Lang.bool true),
    Some "Send new metadata using the ICY protocol.";
    "format", Lang.string_t, Some (Lang.string "mp3"), 
    Some "Content-type (mime) for the format. \
          \"mp3\" is a short-hand for mpeg audio, \"ogg\" for ogg data.";
    "", Lang.source_t, None, None ]

class to_shout p =
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let restart_on_new_track = e Lang.to_bool "restart_on_new_track" in
  let restart_on_crash = e Lang.to_bool "restart_on_crash" in
  let restart_encoder_delay = e Lang.to_int "restart_encoder_delay" in
  let header = e Lang.to_bool "header" in
  let icy = e Lang.to_bool "icy_metadata" in
  let process = List.assoc "process" p in
  let f x =
    if x > 0 then
      Some x
    else
      None
  in
  let bitrate = f (e Lang.to_int "bitrate") in
  let quality =
   let quality = e Lang.to_float "quality" in
     if quality > 0. then
       Some (string_of_float quality)
     else
       None
  in
  let samplerate = f (e Lang.to_int "samplerate") in
  let channels   = f (e Lang.to_int "channels") in
  let autostart = e Lang.to_bool "start" in
  let source = List.assoc "" p in
  let mount = s "mount" in
  let name = s "name" in
  let format = 
    match s "format" with
      | "mp3" -> Cry.mpeg
      | "ogg" -> Cry.ogg_application
      | s -> Cry.content_type_of_string s
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
        | "http" -> Cry.Http
        | "icy" -> Cry.Icy
        | _ ->
            raise (Lang.Invalid_value
                     (v, "valid values are 'http' (icecast) "^
                      "and 'icy' (shoutcast)"))
  in
  let icecast_info =
    {
     Icecast2.
      quality    = quality;
      bitrate    = bitrate;
      channels   = channels;
      samplerate = samplerate
    }
  in
object (self)
  inherit Output.encoded ~autostart ~name:mount ~kind:"output.icecast" source
  inherit
    Icecast2.output ~format ~protocol
      ~icecast_info ~mount ~name ~source p as icecast
  inherit
    External_encoded.base ~restart_on_new_track ~restart_on_crash 
                          ~restart_encoder_delay ~header process
    as base

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
         let m =
           let ret = Hashtbl.create 10 in
           let f (x,y) = Hashtbl.add ret x y in
           Array.iter f a; ret
          in
          match connection with
            | Some c ->
                (try Cry.update_metadata c m with _ -> ())
            (* Do nothing if shout connection isn't available *)
            | None -> ()
        end;
      if restart_on_new_track then
        base#reset_encoder m
      else
        ""

    method output_start =
      Mutex.lock create_m;
      base#external_start External_encoded.initial_meta;
      Mutex.unlock create_m;
      icecast#icecast_start

    method output_stop =
      Mutex.lock create_m;
      base#external_stop;
      Mutex.unlock create_m;
      icecast#icecast_stop

    method output_reset = 
      Mutex.lock create_m;
      base#external_stop;
      icecast#icecast_stop;
      base#external_start External_encoded.initial_meta;
      icecast#icecast_start;
      Mutex.unlock create_m
end

let () =
  Lang.add_operator "output.icecast.external" ~category:Lang.Output
    ~descr:"Output the source's stream to an Icecast2 compatible server \
            using an external encoder."
    proto
    (fun p _ -> ((new to_shout p):>Source.source))

