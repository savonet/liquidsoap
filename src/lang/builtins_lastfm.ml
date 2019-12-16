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

let log = Log.make ["lastfm"; "submit"]

let () =
  let f name stype descr =
    let proto =
      [
        ("user", Lang.string_t, None, None);
        ("password", Lang.string_t, None, None);
        ( "host",
          Lang.string_t,
          Some (Lang.string !Liqfm.Audioscrobbler.base_host),
          Some "Host for audioscrobbling submissions." );
        ( "port",
          Lang.int_t,
          Some (Lang.int !Liqfm.Audioscrobbler.base_port),
          Some "Port for audioscrobbling submissions." );
        ( "length",
          Lang.bool_t,
          Some (Lang.bool false),
          Some
            "Try to submit length information. This operation can be CPU \
             intensive. Value forced to true when used with the \"user\" \
             source type." );
        ("", Lang.metadata_t, None, None);
      ]
    in
    let proto =
      if stype = Liqfm.Played then
        ( "source",
          Lang.string_t,
          Some (Lang.string "broadcast"),
          Some
            "Source for tracks. Should be one of: \"broadcast\", \"user\", \
             \"recommendation\" or \"unknown\". Since liquidsoap is intented \
             for radio broadcasting, this is the default. Sources other than \
             user don't need duration to be set." )
        :: proto
      else proto
    in
    let tasks = Hashtbl.create 1 in
    add_builtin name ~cat:Interaction (* TODO better cat *) ~descr proto
      Lang.unit_t (fun p ->
        let user = Lang.to_string (List.assoc "user" p) in
        let password = Lang.to_string (List.assoc "password" p) in
        let metas = Lang.to_metadata (Lang.assoc "" 1 p) in
        let host = Lang.to_string (List.assoc "host" p) in
        let port = Lang.to_int (List.assoc "port" p) in
        let host = (host, port) in
        let mode =
          if stype = Liqfm.Played then (
            match Lang.to_string (List.assoc "source" p) with
              | "broadcast" -> Liqfm.Broadcast
              | "user" -> Liqfm.User
              | "recommendation" -> Liqfm.Recommendation
              | "unknown" -> Liqfm.Unknown
              | _ ->
                  raise
                    (Lang_errors.Invalid_value
                       (List.assoc "source" p, "unknown lastfm submission mode"))
            )
          else Liqfm.Unknown
        in
        let length = Lang.to_bool (List.assoc "length" p) in
        let length =
          if length = false && mode = Liqfm.User then (
            log#severe
              "length information is required for \"user\" sources, setting to \
               true.";
            true )
          else length
        in
        let task =
          try Hashtbl.find tasks host
          with Not_found ->
            let t = Liqfm.init host in
            Hashtbl.add tasks host t;
            t
        in
        Liqfm.submit (user, password) task length mode stype [metas];
        Lang.unit)
  in
  f "audioscrobbler.submit" Liqfm.Played
    "Submit a played song using the audioscrobbler protocol.";
  f "audioscrobbler.nowplaying" Liqfm.NowPlaying
    "Submit a now playing song using the audioscrobbler protocol."
