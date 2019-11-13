(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type opt_val = [`String of string | `Int of int | `Float of float]

type opts = (string, opt_val) Hashtbl.t

type t = {
  format: string;
  codec: string;
  channels: int;
  samplerate: int Lazy.t;
  options: opts;
}

let string_of_options options =
  let _v = function
    | `String s ->
        Printf.sprintf "%S" s
    | `Int i ->
        string_of_int i
    | `Float f ->
        string_of_float f
  in
  String.concat ","
    (Hashtbl.fold
       (fun k v c ->
         let v = Printf.sprintf "%s=%s" k (_v v) in
         v :: c)
       options [])

let to_string m =
  let opts = string_of_options m.options in
  Printf.sprintf "%%fmpeg(format=%S,codec=%S,ac=%d,ar=%d%s)" m.format m.codec
    m.channels (Lazy.force m.samplerate)
    (if opts = "" then "" else Printf.sprintf ",%s" opts)
