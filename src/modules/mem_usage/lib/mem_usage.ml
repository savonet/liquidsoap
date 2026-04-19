(*
 * SPDX-FileCopyrightText: 2022 - 2024 Savonet team
 *
 * SPDX-License-Identifier: MIT
 *)

type t = {
  total_virtual_memory : int;
  total_physical_memory : int;
  total_used_virtual_memory : int;
  total_used_physical_memory : int;
  process_virtual_memory : int;
  process_physical_memory : int;
  process_private_memory : int;
  process_swapped_memory : int;
}

external info : unit -> t = "ocaml_mem_usage_mem_usage"

let byte_units = ["B"; "kB"; "MB"; "GB"; "TB"; "PB"; "EB"; "ZB"; "YB"]
let bibyte_units = ["B"; "kiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]

let bit_units =
  ["b"; "kbit"; "Mbit"; "Gbit"; "Tbit"; "Pbit"; "Ebit"; "Zbit"; "Ybit"]

let bibit_units =
  ["b"; "kibit"; "Mibit"; "Gibit"; "Tibit"; "Pibit"; "Eibit"; "Zibit"; "Yibit"]

let prettify_bytes ?(float_printer = Printf.sprintf "%.02f") ?(signed = false)
    ?(bits = false) ?(binary = false) bytes =
  let units =
    match (bits, binary) with
      | true, true -> bibit_units
      | true, false -> bit_units
      | false, true -> bibyte_units
      | false, false -> byte_units
  in

  let prefix, bytes =
    if bytes < 0 then ("-", -bytes) else ((if signed then "+" else ""), bytes)
  in

  if bytes = 0 then Printf.sprintf "%s0 %s" prefix (List.hd units)
  else (
    let exponent =
      Float.floor
        (if binary then log (float bytes) /. log 1024.
         else log10 (float bytes) /. 3.)
    in
    let unit_index =
      if List.length units - 1 < int_of_float exponent then
        List.length units - 1
      else int_of_float exponent
    in
    let bytes =
      float bytes /. Float.pow (if binary then 1024. else 1000.) exponent
    in
    Printf.sprintf "%s%s %s" prefix (float_printer bytes)
      (List.nth units unit_index))
