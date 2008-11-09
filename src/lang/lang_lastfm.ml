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

open Lang
open Lang_builtins

let log = Dtools.Log.make ["lastfm";"submit"]

let () =
  add_builtin "lastfm.submit" ~cat:Interaction (* TODO better cat *)
    ~descr:"Submit a song to audioscrobbler."
    [ "user",Lang.string_t,None,None ;
      "password",Lang.string_t,None,None ;
      "source",Lang.string_t,Some (Lang.string "broadcast"),
      Some "Source for tracks. Should be one of: \"broadcast\", \"user\", \"recommendation\" \
            or \"unknown\". Since liquidsoap is intented for radio broadcasting, this is the default. \
            Sources other than user don't need duration to be set.";
      "length",Lang.bool_t,Some (Lang.bool false),
      Some "Try to submit length information. This operation can be CPU intensive. \
            Value forced to true when used with the \"user\" source type.";
      "",Lang.metadata_t,None,None ]
    Lang.unit_t
    (fun p ->
       let user = Lang.to_string (List.assoc "user" p) in
       let password = Lang.to_string (List.assoc "password" p) in
       let metas = Lang.to_metadata (Lang.assoc "" 1 p) in
       let mode = 
         match Lang.to_string (List.assoc "source" p) with
           | "broadcast" -> Liqfm.Broadcast
           | "user" -> Liqfm.User
           | "recommendation" -> Liqfm.Recommendation
           | "unknown" -> Liqfm.Unknown
           | _ -> failwith "unknown lastfm submission mode"
       in
       let length = Lang.to_bool (List.assoc "length" p) in
       let length = 
         if length = false && mode = Liqfm.User then
           begin
             log#f 2 "length information is required for \"user\" sources, setting to true.";
             true
           end
         else
           length
       in
         Liqfm.submit (user,password) length mode [metas];
         Lang.unit)
