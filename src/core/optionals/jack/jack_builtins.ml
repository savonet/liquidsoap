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

let flag_of_string ~pos = function
  | "is_physical" -> `IsPhysical
  | "can_monitor" -> `CanMonitor
  | "is_terminal" -> `IsTerminal
  | s ->
      Lang.raise_error ~pos
        ~message:
          (Printf.sprintf
             "Invalid JACK port flag %S. Expected one of: is_physical, \
              can_monitor, is_terminal."
             s)
        "jack"

let extra_flags_proto =
  [
    ( "flags",
      Lang.list_t Lang.string_t,
      Some (Lang.list []),
      Some
        "Additional port flags to filter by. Valid values: `\"is_physical\"`, \
         `\"can_monitor\"`, `\"is_terminal\"`." );
  ]

let get_extra_flags p =
  let pos = Lang.pos p in
  List.map (flag_of_string ~pos)
    (List.map Lang.to_string (Lang.to_list (List.assoc "flags" p)))

let pattern_proto =
  [
    ( "pattern",
      Lang.string_t,
      Some (Lang.string ""),
      Some "Port name pattern. Empty string matches all ports." );
  ]

let port_type_proto =
  [
    ( "type",
      Lang.string_t,
      Some (Lang.string "audio"),
      Some "Port type to filter by: `\"audio\"` or `\"midi\"`." );
  ]

let port_type_of_string ~pos = function
  | "audio" -> Jack.default_audio_type
  | "midi" -> Jack.default_midi_type
  | s ->
      Lang.raise_error ~pos
        ~message:
          (Printf.sprintf
             "Invalid JACK port type %S. Expected \"audio\" or \"midi\"." s)
        "jack"

let make_port_values to_value ~base_flag ~pattern ~port_type ~extra_flags server
    =
  with_client server (fun c ->
      let client = Option.get c#client in
      let get_client () = c#client in
      Lang.list
        (List.map
           (fun port_name -> to_value { Jack_io.port_name; get_client })
           (Jack.get_ports ~pattern ~port_type ~flags:(base_flag :: extra_flags)
              client)))

let _ =
  Lang.add_builtin ~base:jack_server_m "connect" ~category:`Liquidsoap
    ~descr:"Connect a JACK output port to a JACK input port."
    (server_proto
    @ [
        ("", Jack_io.Jack_input_port_base.t, None, Some "Input port.");
        ("", Jack_io.Jack_output_port_base.t, None, Some "Output port.");
      ])
    Lang.unit_t
    (fun p ->
      let server = get_server p in
      let input = Lang.demeth (Lang.assoc "" 1 p) in
      let output = Lang.demeth (Lang.assoc "" 2 p) in
      let input = Jack_io.Jack_input_port_base.of_value input in
      let output = Jack_io.Jack_output_port_base.of_value output in
      with_client server (fun c ->
          Jack.port_connect (Option.get c#client) output.Jack_io.port_name
            input.Jack_io.port_name);
      Lang.unit)

let _ =
  Lang.add_builtin ~base:jack_server_m "get_input_ports" ~category:`Liquidsoap
    ~descr:"Return the JACK input ports matching the given pattern and type."
    (server_proto @ pattern_proto @ port_type_proto @ extra_flags_proto)
    (Lang.list_t Jack_io.Jack_input_port_base.t)
    (fun p ->
      let server = get_server p in
      let pattern = Lang.to_string (List.assoc "pattern" p) in
      let port_type =
        port_type_of_string ~pos:(Lang.pos p)
          (Lang.to_string (List.assoc "type" p))
      in
      let extra_flags = get_extra_flags p in
      make_port_values Jack_io.Jack_input_port_base.to_value ~base_flag:`IsInput
        ~pattern ~port_type ~extra_flags server)

let _ =
  Lang.add_builtin ~base:jack_server_m "get_output_ports" ~category:`Liquidsoap
    ~descr:"Return the JACK output ports matching the given pattern and type."
    (server_proto @ pattern_proto @ port_type_proto @ extra_flags_proto)
    (Lang.list_t Jack_io.Jack_output_port_base.t)
    (fun p ->
      let server = get_server p in
      let pattern = Lang.to_string (List.assoc "pattern" p) in
      let port_type =
        port_type_of_string ~pos:(Lang.pos p)
          (Lang.to_string (List.assoc "type" p))
      in
      let extra_flags = get_extra_flags p in
      make_port_values Jack_io.Jack_output_port_base.to_value
        ~base_flag:`IsOutput ~pattern ~port_type ~extra_flags server)
