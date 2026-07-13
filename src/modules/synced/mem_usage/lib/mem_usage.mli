(*
 * SPDX-FileCopyrightText: 2022 - 2024 Savonet team
 *
 * SPDX-License-Identifier: MIT
 *)

(** All values are in bytes.

    - [total_virtual_memory]: RAM + swap on Linux and macOS, commit limit (RAM +
      page files) on Windows.
    - [total_used_virtual_memory]: used RAM + used swap (used commit charge on
      Windows).
    - [total_used_physical_memory]: used RAM. Excludes easily reclaimable cache
      where the platform exposes it (buffers on Linux, inactive pages on macOS).
    - [process_virtual_memory]: process address-space usage (committed private
      bytes on Windows).
    - [process_physical_memory]: process resident set / working set.
    - [process_private_memory]: resident memory private to the process.
    - [process_swapped_memory]: process memory currently swapped out
      (approximated on Windows by committed-but-non-resident private bytes).

    On FreeBSD the [process_*] fields are always [0]: they are read from
    Linux-specific procfs files. *)
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
