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

open Aacplus_encoded

(** Output a AAC+ stream to an icecast server *)

let no_mount = "Use [name]"
let no_name = "Use [mount]"

let proto =
  (Icecast2.proto ~no_mount ~no_name ~format:"audio/aacp") @ Output.proto @
  [ "samplerate", Lang.int_t, Some (Lang.int 44100), None;
    "bitrate", Lang.int_t, Some (Lang.int 64), None;
    "", Lang.source_t, None, None ]

let no_multicast = "no_multicast"

class to_shout p =
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let samplerate = e Lang.to_int "samplerate" in
  let bitrate = e Lang.to_int "bitrate" in

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
  let channels = Fmt.channels () in
  let icecast_info =
    {
     Icecast2.
      quality    = None;
      bitrate    = Some bitrate;
      channels   = Some channels;
      samplerate = Some samplerate
    }
  in

  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let on_start =
    let f = List.assoc "on_start" p in
      fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
      fun () -> ignore (Lang.apply f [])
  in
object (self)
  inherit Output.encoded ~autostart ~name:mount 
                         ~infallible ~on_start ~on_stop
                         ~kind:"output.icecast" source
  inherit Icecast2.output ~icecast_info ~mount ~name ~source p as icecast
  inherit base ~bitrate ~samplerate as base

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
      let m =
         let ret = Hashtbl.create 10 in
         let f (x,y) = Hashtbl.add ret x y in
         Array.iter f a; ret  
      in
      match Cry.get_status connection with
        | Cry.Connected _ ->
            (try Cry.update_metadata connection m ; "" with _ -> "")
        (* Do nothing if shout connection isn't available *)
        | Cry.Disconnected -> ""

  method output_start =
    icecast#icecast_start ;
    base#output_start 

  method output_stop = icecast#icecast_stop

  method output_reset = 
    self#output_stop;
    self#output_start

end

let () =
  Lang.add_operator "output.icecast.aacplus" ~category:Lang.Output
    ~descr:"Output the source's stream to an icecast2-compatible server \
            in AAC+ format."
    proto
    (fun p _ -> ((new to_shout p):>Source.source))
