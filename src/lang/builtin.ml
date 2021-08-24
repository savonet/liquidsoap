(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

type category =
  | Sys
  | Math
  | String
  | List
  | Bool
  | Pair
  | Liq
  | Control
  | Interaction
  | Other
  | FFmpegFilter

let string_of_category = function
  | Sys -> "System"
  | Math -> "Math"
  | String -> "String"
  | List -> "List"
  | Pair -> "Pair"
  | Bool -> "Bool"
  | Liq -> "Liquidsoap"
  | Control -> "Control"
  | Interaction -> "Interaction"
  | Other -> "Other"
  | FFmpegFilter -> "FFmpeg Filter"

let add_builtin ~cat ~descr ?(meth = []) ?flags name proto ret_t f =
  let ret_t =
    if meth = [] then ret_t
    else (
      let meth = List.map (fun (l, t, d, _) -> (l, t, d)) meth in
      Lang.method_t ret_t meth)
  in
  let f =
    if meth = [] then f
    else (
      let meth = List.map (fun (l, _, _, f) -> (l, f)) meth in
      fun p -> Lang.meth (f p) meth)
  in
  Lang.add_builtin ~category:(string_of_category cat) ~descr ?flags name proto
    ret_t f
