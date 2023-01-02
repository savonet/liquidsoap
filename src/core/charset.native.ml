(*****************************************************************************

    Liquidsoap, a programmable audio stream generator.
    Copyright 2003-2023 Savonet team

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

include Charset_base

let log = Log.make ["charset"]

let convert ?(fail = false) ?(source = `UTF_8) ?(target = `UTF_8) s =
  if source <> `UTF_8 then
    if fail then raise (Unsupported_encoding source)
    else log#important "Conversion from %s is not supported." (to_string source);
  if target <> `UTF_8 then
    if fail then raise (Unsupported_encoding target)
    else log#important "Conversion to %s is not supported." (to_string target);
  s
