(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let jack_m = Lang.add_module "jack"
let jack_server_m = Lang.add_module ~base:jack_m "server"

let server_proto =
  [
    ( "server",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "JACK server to query. Uses default server if `null`." );
  ]

let get_server p =
  match Lang.to_valued_option Lang.to_string (List.assoc "server" p) with
    | Some "" -> None
    | s -> s

let with_client server f =
  let c = new Jack_io.jack_client ~id:"tmp" server in
  c#open_client;
  let result = f c in
  c#close;
  result

let _ =
  Lang.add_builtin ~base:jack_server_m "buffer_size" ~category:`Liquidsoap
    ~descr:"Return the current buffer size of the JACK server." server_proto
    Lang.int_t (fun p ->
      let server = get_server p in
      Lang.int (with_client server (fun c -> c#buffer_size)))

let _ =
  Lang.add_builtin ~base:jack_server_m "sample_rate" ~category:`Liquidsoap
    ~descr:"Return the current sample rate of the JACK server." server_proto
    Lang.int_t (fun p ->
      let server = get_server p in
      Lang.int (with_client server (fun c -> c#sample_rate)))
