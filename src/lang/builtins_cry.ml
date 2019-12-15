(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Lang_builtins

let log = Log.make ["icy"; "update_metadata"]

let () =
  let user_agent =
    Lang.product (Lang.string "User-Agent") (Lang.string Http.user_agent)
  in
  add_builtin "icy.update_metadata" ~cat:Interaction
    ~descr:"Update metata on an icecast mountpoint using the ICY protocol."
    [
      ("host", Lang.string_t, Some (Lang.string "localhost"), None);
      ("port", Lang.int_t, Some (Lang.int 8000), None);
      ("user", Lang.string_t, Some (Lang.string "source"), None);
      ("password", Lang.string_t, Some (Lang.string "hackme"), None);
      ( "mount",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Source mount point. Mandatory when streaming to icecast." );
      ( "icy_id",
        Lang.int_t,
        Some (Lang.int 1),
        Some "Shoutcast source ID. Only supported by Shoutcast v2." );
      ( "protocol",
        Lang.string_t,
        Some (Lang.string "http"),
        Some "Protocol to use. One of: \"icy\", \"http\" or \"https\"" );
      ( "encoding",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Encoding used to send metadata, default (UTF-8) if empty." );
      ( "headers",
        Lang.metadata_t,
        Some (Lang.list ~t:Lang.metadata_t [user_agent]),
        Some "Additional headers." );
      ("", Lang.metadata_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let user = Lang.to_string (List.assoc "user" p) in
      let password = Lang.to_string (List.assoc "password" p) in
      let mount = Lang.to_string (List.assoc "mount" p) in
      let icy_id = Lang.to_int (List.assoc "icy_id" p) in
      let metas = Lang.to_metadata (Lang.assoc "" 1 p) in
      let out_enc =
        match Lang.to_string (List.assoc "encoding" p) with
          | "" -> None
          | s -> Some s
      in
      let metas =
        let ret = Hashtbl.create (Hashtbl.length metas) in
        Hashtbl.iter
          (fun x y -> Hashtbl.add ret x (Configure.recode_tag ?out_enc y))
          metas;
        ret
      in
      let host = Lang.to_string (List.assoc "host" p) in
      let port = Lang.to_int (List.assoc "port" p) in
      let headers =
        List.map
          (fun v ->
            let f (x, y) = (Lang.to_string x, Lang.to_string y) in
            f (Lang.to_product v))
          (Lang.to_list (List.assoc "headers" p))
      in
      let headers =
        let h = Hashtbl.create 10 in
        List.iter (fun (x, y) -> Hashtbl.add h x y) headers;
        h
      in
      let protocol =
        let v = List.assoc "protocol" p in
        match Lang.to_string v with
          | "icy" -> Cry.Icy
          | "http" -> Cry.Http Cry.Source (* Verb doesn't matter here. *)
          | "https" -> Cry.Https Cry.Source
          | _ ->
              raise
                (Lang_errors.Invalid_value
                   (v, "protocol should be one of: 'icy', 'http' or 'https'."))
      in
      let mount =
        match protocol with
          | Cry.Icy -> Cry.Icy_id icy_id
          | _ -> Cry.Icecast_mount mount
      in
      begin
        try
          Cry.manual_update_metadata ~host ~port ~protocol ~user ~password
            ~mount ~headers metas
        with e ->
          log#severe "Manual metadata update failed: %s" (Printexc.to_string e)
      end;
      Lang.unit)
