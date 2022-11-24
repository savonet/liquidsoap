(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let _ =
  Lang.add_builtin ~base:Modules.file_mime "libmagic" ~category:`File
    ~descr:"Get the MIME type of a file, according to libmagic."
    [("", Lang.string_t, None, None)]
    (Lang.nullable_t Lang.string_t)
    (fun p ->
      (* TODO is that really impossible ? *)
      let file = Lang.to_string (Lang.assoc "" 1 p) in
      match Liqmagic.file_mime file with
        | Some "" -> Lang.null
        | Some s -> Lang.string s
        | None -> assert false)
