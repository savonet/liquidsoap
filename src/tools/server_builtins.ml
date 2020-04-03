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

let () =
  let add = Server.add ~ns:[] in
  add "version" ~descr:"Display liquidsoap version." (fun _ ->
      "Liquidsoap " ^ Configure.version);
  add "request.all"
    ~descr:
      "Get the identifiers of all requests, in use or not. Destroyed requests \
       will remain there only for some limited time." (fun _ ->
      String.concat " " (List.map string_of_int (Request.all_requests ())));
  add "request.alive"
    ~descr:"Get the identifiers of requests that are still in use." (fun _ ->
      String.concat " " (List.map string_of_int (Request.alive_requests ())));
  add "request.on_air" ~descr:"Get the identifiers of requests that are on air."
    (fun _ ->
      String.concat " " (List.map string_of_int (Request.on_air_requests ())));
  add "request.resolving"
    ~descr:"Get the identifiers of requests that are being prepared." (fun _ ->
      String.concat " " (List.map string_of_int (Request.resolving_requests ())));
  add "request.trace" ~usage:"request.trace <rid>"
    ~descr:"Print the log associated to a request." (fun args ->
      let id = int_of_string args in
      match Request.from_id id with
        | Some r ->
            let log = Request.get_log r in
            Request.string_of_log log
        | None -> "No such request.");
  add "request.metadata" ~usage:"request.metadata <rid>"
    ~descr:"Display the metadata associated to a request." (fun args ->
      let id = int_of_string args in
      match Request.from_id id with
        | Some r ->
            let m = Request.get_all_metadata r in
            Request.string_of_metadata m
        | None -> "No such request.");
  add "uptime" ~descr:"Print the uptime for this instance." (fun _ ->
      let date = int_of_float (Utils.uptime ()) in
      Printf.sprintf "%dd %02dh %02dm %02ds"
        (date / (24 * 60 * 60))
        (date mod (24 * 60 * 60) / (60 * 60))
        (date mod (60 * 60) / 60)
        (date mod 60))
