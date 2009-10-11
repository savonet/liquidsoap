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

(** Send Ogg data through a shout connection *)

let no_mount = "Use [name].ogg"
let no_name = "Use [mount]"

let proto =
  (Icecast2.proto ~no_mount ~no_name ~format:"ogg") @ Output.proto @
  [ "start", Lang.bool_t, Some (Lang.bool true),
    Some "Start output threads on operator initialization." ;
    "", Lang.source_t, None, None ]

class to_shout ~skeleton ~streams 
               ~icecast_info p =

  let s v = Lang.to_string (List.assoc v p) in

  let name = s  "name" in
  let mount = s "mount" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 ((List.assoc "mount" p),
                  "Either name or mount must be defined"))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then name ^ ".ogg" else mount
  in

  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let on_start =
    let f = List.assoc "on_start" p in
      fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
      fun () -> ignore (Lang.apply f [])
  in

  let source = List.assoc "" p in
  let autostart = Lang.to_bool (List.assoc "start" p) in

object (self)
  inherit Output.encoded ~autostart 
            ~infallible ~on_start ~on_stop
            ~name:mount ~kind:"output.icecast" source
  inherit Icecast2.output
    ~mount ~name ~icecast_info 
    ~source p as icecast
  inherit Ogg_output.base ~skeleton streams as ogg

  method output_start =
    ogg#ogg_start;
    icecast#icecast_start 

  method reset_encoder m = 
    let m =
      let f x y z =
        (x,y)::z
      in
      Hashtbl.fold f m []
    in
    if encoder <> None then
      ogg#reset_stream m
    else
      ""

  method output_stop =
    if encoder <> None then
     begin
      let b = ogg#end_of_stream in
      ogg#ogg_stop;
      (* If possible, output the end of stream so that it's well-formed. *)
      if Cry.get_status connection <> Cry.Disconnected then icecast#send b
     end;
    icecast#icecast_stop

  method output_reset = 
    self#output_stop;
    self#output_start
end

