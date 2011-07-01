(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

let log = Dtools.Log.make ["icy";"update_metadata"]

let () =
  let user_agent = Lang.product (Lang.string "User-Agent")
                                (Lang.string Http.user_agent)
  in
  add_builtin "icy.update_metadata" ~cat:Interaction ~descr:"Update metata on an icecast mountpoint \
                                                       using the ICY protocol."
    ["host", Lang.string_t, Some (Lang.string "localhost"), None;
     "port", Lang.int_t, Some (Lang.int 8000), None;
     "user", Lang.string_t, Some (Lang.string "source"), None;
     "password", Lang.string_t, Some (Lang.string "hackme"), None;
     "mount", Lang.string_t, None , None;
     "protocol", Lang.string_t, Some (Lang.string "http"), 
     Some "Protocol to use. One of: \"icy\" or \"http\"";
     "encoding", Lang.string_t, Some (Lang.string ""),
     Some "Encoding used to send metadata, default (UTF-8) if empty." ;
     "headers", Lang.metadata_t,
     Some (Lang.list (Lang.product_t Lang.string_t Lang.string_t) [user_agent]),
     Some "Additional headers." ;
     "",Lang.metadata_t,None,None ] Lang.unit_t
    (fun p ->
      let user = Lang.to_string (List.assoc "user" p) in
      let password = Lang.to_string (List.assoc "password" p) in
      let mount = Lang.to_string (List.assoc "mount" p) in
      let metas = Lang.to_metadata (Lang.assoc "" 1 p) in
      let out_enc =
        match Lang.to_string (List.assoc  "encoding" p) with
          | "" -> None
          | s -> Some s
      in
      let metas = 
        let ret = Hashtbl.create (Hashtbl.length metas) in
        Hashtbl.iter 
          (fun x y -> Hashtbl.add ret x (Configure.recode_tag ?out_enc y))
          metas ;
        ret
      in
      let host = Lang.to_string (List.assoc "host" p) in
      let port = Lang.to_int (List.assoc "port" p) in
      let headers = 
        List.map (fun v ->
                let f (x,y) =
                  Lang.to_string x, Lang.to_string y
                in
                f (Lang.to_product v))
             (Lang.to_list (List.assoc "headers" p))
      in
      let headers = 
        let h = Hashtbl.create 10 in
        List.iter (fun (x,y) -> Hashtbl.add h x y) headers ;
        h
      in
      let protocol = 
        let v = List.assoc "protocol" p in
        match Lang.to_string v with
          | "icy"  -> Cry.Icy
          | "http" -> Cry.Http
          | _      -> 
              raise (Lang.Invalid_value (v, "protocol should be one of: \
                                             'icy' or 'http'."))
      in
      begin
       try
         Cry.manual_update_metadata
           ~host ~port ~protocol
           ~user ~password
           ~mount ~headers metas
       with
         | e -> log#f 2 "Manual metadata update failed: %s" (Utils.error_message e)
      end ;
      Lang.unit)
