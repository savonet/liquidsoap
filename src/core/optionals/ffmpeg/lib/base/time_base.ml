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

type t = Avutil.rational

let main () = { Avutil.num = 1; den = Lazy.Mutexed.force Frame.main_rate }
let audio () = { Avutil.num = 1; den = Lazy.Mutexed.force Frame.audio_rate }
let video () = { Avutil.num = 1; den = Lazy.Mutexed.force Frame.video_rate }
let is_valid { Avutil.num; den } = num > 0 && den > 0
let to_string { Avutil.num; den } = Printf.sprintf "%d/%d" num den

exception Out_of_range

let checked_add a b =
  let s = a + b in
  if (a >= 0 && b >= 0 && s < 0) || (a < 0 && b < 0 && s >= 0) then
    raise Out_of_range;
  s

let checked_mul a b =
  if a <> 0 && abs b > max_int / abs a then raise Out_of_range;
  a * b

(* [mul_div r a b] for [0 <= r < b] is the quotient and remainder of
   [r × a / b], computed bit by bit so that no product exceeds [2b]. *)
let mul_div r a b =
  let rec go bit q rem =
    if bit < 0 then (q, rem)
    else (
      let q = checked_mul q 2 in
      let q, rem =
        if rem >= b - rem then (q + 1, rem - (b - rem)) else (q, rem * 2)
      in
      let q, rem =
        if (a lsr bit) land 1 = 1 then
          if rem >= b - r then (q + 1, rem - (b - r)) else (q, rem + r)
        else (q, rem)
      in
      go (bit - 1) q rem)
  in
  go (Sys.int_size - 2) 0 0

let rescale ~src ~dst t =
  if not (is_valid src && is_valid dst) then
    invalid_arg
      (Printf.sprintf "Invalid time base %s -> %s" (to_string src)
         (to_string dst));
  try
    let a = checked_mul src.Avutil.num dst.Avutil.den in
    let b = checked_mul src.Avutil.den dst.Avutil.num in
    let negative = t < 0 in
    let t = if negative then -t else t in
    let q, r = (t / b, t mod b) in
    let q', rem = mul_div r a b in
    let result = checked_add (checked_mul q a) q' in
    let result = if rem >= b - rem then checked_add result 1 else result in
    Some (if negative then -result else result)
  with Out_of_range -> None

let rescale_int64 ~src ~dst t =
  if
    Int64.compare t (Int64.of_int max_int) > 0
    || Int64.compare t (Int64.of_int (-max_int)) < 0
  then None
  else rescale ~src ~dst (Int64.to_int t)

let to_main ~src t = rescale ~src ~dst:(main ()) t
