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

(* Some of the code below was borrowed from opam. *)

let dumb_term =
  lazy (try Sys.getenv "TERM" = "dumb" with Not_found -> Sys.win32)

type color_conf = [ `Always | `Never | `Auto ]

let color_conf : color_conf ref = ref `Auto

let color =
  let auto =
    lazy
      (try Unix.isatty Unix.stdout && not (Lazy.force dumb_term)
       with _ -> false)
  in
  fun () ->
    match !color_conf with
      | `Always -> true
      | `Never -> false
      | `Auto -> Lazy.force auto

type text_style =
  [ `bold
  | `underline
  | `crossed
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

let style_code (c : text_style) =
  match c with
    | `bold -> "01"
    | `underline -> "04"
    | `crossed -> "09"
    | `black -> "30"
    | `red -> "31"
    | `green -> "32"
    | `yellow -> "33"
    | `blue -> "1;34" (* most terminals make blue unreadable unless bold *)
    | `magenta -> "35"
    | `cyan -> "36"
    | `white -> "37"

let colorize styles s =
  if not (color ()) then s
  else
    Printf.sprintf "\027[%sm%s\027[0m"
      (String.concat ";" (List.map style_code styles))
      s

let start_color styles =
  if not (color ()) then ""
  else
    Printf.sprintf "\027[%sm" (String.concat ";" (List.map style_code styles))

let stop_color () = if not (color ()) then "" else "\027[0m"
