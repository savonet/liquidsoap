(*
 * Copyright (C) 2003-2008 Samuel Mimram
 *           (C) 2006-2010 The Savonet Team
 *
 * Ocaml-faad is free software; you can redistribute it and/or modify<F12>
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-faad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-faad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t

exception Error of int
exception Failed

external error_message : int -> string = "ocaml_faad_get_error_message"

let () =
  Callback.register_exception "ocaml_faad_exn_error" (Error 0);
  Callback.register_exception "ocaml_faad_exn_failed" Failed

external min_bytes_per_channel : unit -> int
  = "ocaml_faad_min_bytes_per_channel"

let min_bytes_per_channel = min_bytes_per_channel ()

external create : unit -> t = "ocaml_faad_open"

external init : t -> Bytes.t -> int -> int -> int * int * int
  = "ocaml_faad_init"

external decode : t -> Bytes.t -> int -> int -> int * float array array
  = "ocaml_faad_decode"

external post_sync_reset : t -> unit = "ocaml_faad_post_seek_reset"

let find_frame buf =
  let i = ref 0 in
  let found = ref false in
  while !i < String.length buf - 1 && not !found do
    if buf.[!i] = '\255' then
      if int_of_char buf.[!i + 1] land 0xf6 = 0xf0 then found := true
      else incr i
    else incr i
  done;
  if !found then !i else raise Not_found

module Mp4 = struct
  type decoder = t
  type t
  type track = int
  type sample = int
  type read = bytes -> int -> int -> int

  let is_mp4 s =
    assert (String.length s >= 8);
    s.[4] = 'f' && s.[5] = 't' && s.[6] = 'y' && s.[7] = 'p'

  external open_read :
    bool ->
    read ->
    (Bytes.t -> int) option ->
    (int -> int) option ->
    (unit -> int) option ->
    t = "ocaml_faad_mp4_open_read"

  external open_read_fd : bool -> Unix.file_descr -> t
    = "ocaml_faad_mp4_open_read_fd"

  let openfile ?write ?seek ?trunc read = open_read false read write seek trunc
  let openfile_fd = open_read_fd false

  external seek : t -> track -> int -> int * int = "ocaml_faad_mp4_seek"

  let seek mp4 track offset =
    let sample, to_skip = seek mp4 track offset in
    if sample < 0 then raise Failed else (sample, to_skip)

  external tracks : t -> int = "ocaml_faad_mp4_total_tracks"
  external find_aac_track : t -> track = "ocaml_faad_mp4_find_aac_track"
  external init : t -> decoder -> track -> int * int = "ocaml_faad_mp4_init"
  external samples : t -> track -> int = "ocaml_faad_mp4_num_samples"

  external read_sample : t -> track -> sample -> string
    = "ocaml_faad_mp4_read_sample"

  external decode : t -> track -> sample -> decoder -> float array array
    = "ocaml_faad_mp4_decode"

  external metadata : t -> (string * string) array = "ocaml_faad_mp4_metadata"
end
