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

let null = "_0_null"
let any = "_0_any"
let eval = "_0_eval"
let json_parser = "_0_json_parser"
let xml_parser = "_0_xml_parser"
let yaml_parser = "_0_yaml_parser"
let sqlite_row_parser = "_0_sqlite_row_parser"
let pattern_var = Printf.sprintf "_%d_pat"
let annotation_var = Printf.sprintf "_0_ann_%d"
