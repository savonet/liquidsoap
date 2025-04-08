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

val info : unit -> t

val prettify_bytes :
  ?float_printer:(float -> string) ->
  ?signed:bool ->
  ?bits:bool ->
  ?binary:bool ->
  int ->
  string
