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

external poll_c :
  Unix.file_descr array ->
  Unix.file_descr array ->
  Unix.file_descr array ->
  float ->
  Unix.file_descr array * Unix.file_descr array * Unix.file_descr array
  = "caml_liquidsoap_poll"

(* EINTR is handled in the C stub. *)
let poll r w e timeout =
  let r, w, e =
    poll_c (Array.of_list r) (Array.of_list w) (Array.of_list e) timeout
  in
  (Array.to_list r, Array.to_list w, Array.to_list e)

let rec select r w e timeout =
  try Unix.select r w e timeout
  with Unix.Unix_error (Unix.EINTR, _, _) -> select r w e timeout

let rec read fd buf ofs len =
  try Unix.read fd buf ofs len
  with Unix.Unix_error (Unix.EINTR, _, _) -> read fd buf ofs len

let rec write fd buf ofs len =
  try Unix.write fd buf ofs len
  with Unix.Unix_error (Unix.EINTR, _, _) -> write fd buf ofs len

let rec accept ?cloexec sock =
  try Unix.accept ?cloexec sock
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept ?cloexec sock

let rec recv sock buf ofs len flags =
  try Unix.recv sock buf ofs len flags
  with Unix.Unix_error (Unix.EINTR, _, _) -> recv sock buf ofs len flags

let rec recvfrom sock buf ofs len flags =
  try Unix.recvfrom sock buf ofs len flags
  with Unix.Unix_error (Unix.EINTR, _, _) -> recvfrom sock buf ofs len flags

let rec send sock buf ofs len flags =
  try Unix.send sock buf ofs len flags
  with Unix.Unix_error (Unix.EINTR, _, _) -> send sock buf ofs len flags

let rec sendto sock buf ofs len flags addr =
  try Unix.sendto sock buf ofs len flags addr
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    sendto sock buf ofs len flags addr

let rec mkdir path perm =
  try Unix.mkdir path perm
  with Unix.Unix_error (Unix.EINTR, _, _) -> mkdir path perm
