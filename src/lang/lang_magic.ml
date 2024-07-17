(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang
open Lang_builtins

let () =
  add_builtin "file.mime" ~cat:Liq (* TODO bad category *)
    ~descr:"Get the MIME type of a file, according to libmagic."
    [ "",Lang.string_t,None,None ] Lang.string_t
    (fun p ->
       let file = Lang.to_string (Lang.assoc "" 1 p) in
         match Configure.file_mime file with
           | Some s -> Lang.string s
           | None -> assert false (* TODO is that really impossible ? *))
