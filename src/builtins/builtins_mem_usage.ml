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

let _ =
  let mem_usage_t =
    Lang.record_t
      [
        ("total_virtual_memory", Lang.int_t);
        ("total_physical_memory", Lang.int_t);
        ("total_used_virtual_memory", Lang.int_t);
        ("total_used_physical_memory", Lang.int_t);
        ("process_virtual_memory", Lang.int_t);
        ("process_physical_memory", Lang.int_t);
        ("process_private_memory", Lang.int_t);
        ("process_swapped_memory", Lang.int_t);
      ]
  in
  let mem_usage
      {
        Mem_usage.total_virtual_memory;
        total_physical_memory;
        total_used_virtual_memory;
        total_used_physical_memory;
        process_virtual_memory;
        process_physical_memory;
        process_private_memory;
        process_swapped_memory;
      } =
    Lang.record
      [
        ("total_virtual_memory", Lang.int total_virtual_memory);
        ("total_physical_memory", Lang.int total_physical_memory);
        ("total_used_virtual_memory", Lang.int total_used_virtual_memory);
        ("total_used_physical_memory", Lang.int total_used_physical_memory);
        ("process_virtual_memory", Lang.int process_virtual_memory);
        ("process_physical_memory", Lang.int process_physical_memory);
        ("process_private_memory", Lang.int process_private_memory);
        ("process_swapped_memory", Lang.int process_swapped_memory);
      ]
  in
  let runtime_mem_usage =
    Lang.add_builtin ~base:Modules.runtime "memory" ~category:`System
      ~descr:"Returns information about the system and process' memory." []
      mem_usage_t (fun _ -> mem_usage (Mem_usage.info ()))
  in
  Lang.add_builtin ~base:runtime_mem_usage "prettify_bytes" ~category:`String
    ~descr:"Returns a human-redable description of an amount of bytes."
    [
      ( "float_printer",
        Lang.nullable_t (Lang.fun_t [(false, "", Lang.float_t)] Lang.string_t),
        Some Lang.null,
        None );
      ("signed", Lang.nullable_t Lang.bool_t, Some Lang.null, None);
      ("bits", Lang.nullable_t Lang.bool_t, Some Lang.null, None);
      ("binary", Lang.nullable_t Lang.bool_t, Some Lang.null, None);
      ("", Lang.int_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let float_printer =
        Lang.to_valued_option
          (fun v f -> Lang.to_string (Lang.apply v [("", Lang.float f)]))
          (List.assoc "float_printer" p)
      in
      let signed = Lang.to_valued_option Lang.to_bool (List.assoc "signed" p) in
      let bits = Lang.to_valued_option Lang.to_bool (List.assoc "bits" p) in
      let binary = Lang.to_valued_option Lang.to_bool (List.assoc "binary" p) in
      let bytes = Lang.to_int (List.assoc "" p) in
      Lang.string
        (Mem_usage.prettify_bytes ?float_printer ?signed ?bits ?binary bytes))
