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

(* Wrapper around Unix.read that automatically retries on EINTR.
   EINTR occurs when a signal is delivered during a blocking read. *)
let rec read fd buf ofs len =
  try Unix.read fd buf ofs len
  with Unix.Unix_error (Unix.EINTR, _, _) -> read fd buf ofs len

(* Wrapper around Unix_utils.write that automatically retries on EINTR.
   EINTR occurs when a signal is delivered during a blocking write. *)
let rec write fd buf ofs len =
  try Unix.write fd buf ofs len
  with Unix.Unix_error (Unix.EINTR, _, _) -> write fd buf ofs len
