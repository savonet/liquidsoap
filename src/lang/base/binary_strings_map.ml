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

(** Strings longer than this or containing invalid UTF-8 are treated as binary.
*)
let max_printable_length = ref 4096

(** Detect binary blobs. When [utf8] is [true] (default), any non-UTF-8 string
    is considered binary — suitable for internally-managed strings that should
    always be UTF-8. When [utf8] is [false], only the length is checked —
    suitable for externally-sourced strings that may be in any text encoding. *)
let is_binary ?(utf8 = true) value =
  if String.length value > !max_printable_length then true
  else utf8 && not (String.is_valid_utf_8 value)
