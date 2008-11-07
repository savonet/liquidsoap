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

(** Send Ogg data through a shout connection *)

let no_mount = "Use [name].ogg"
let no_name = "Use [mount]"

let proto =
  Icecast2.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "", Lang.source_t, None, None ]

class to_shout ~skeleton ~streams ~bitrate p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

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

  let source = List.assoc "" p in

object (self)
  inherit [Ogg_encoder.t] Icecast2.output 
    ~bitrate ~mount ~name ~source p as super
  inherit Ogg_output.base ~skeleton streams as ogg

  method output_start =
    ogg#output_start;
    super#output_start 

  method reset_encoder m = 
    let m =
      let f x y z =
        (x,y)::z
      in
      Hashtbl.fold f m []
    in
    ogg#reset_stream m

  method output_stop =
    let b = ogg#end_of_stream in
    ogg#output_stop;
    super#send b;
    super#output_stop
end

