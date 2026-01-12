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

let id3v2 = Lang.add_module ~base:Modules.metadata "id3v2"

let _ =
  Lang.add_builtin ~base:id3v2 "render" ~category:`String
    ~descr:"Return a string representation of a id3v2 metadata tag"
    [
      ("", Lang.metadata_t, None, None);
      ( "version",
        Lang.int_t,
        Some (Lang.int 3),
        Some "Tag version. One of: 3 or 4" );
    ]
    Lang.string_t
    (fun p ->
      let m = Frame.Metadata.to_list (Lang.to_metadata (List.assoc "" p)) in
      let version = Lang.to_int (List.assoc "version" p) in
      Lang.string (Utils.id3v2_of_metadata ~version m))

let parse = Lang.add_module ~base:Modules.metadata "parse"

let _ =
  Lang.add_builtin ~base:parse "amplify" ~category:`String
    ~descr:
      "Parse an amplify metadata. Parsing is the same as in the `amplify` \
       operator. Metadata can be of the form: \"<db> dB\" for a decibel-based \
       value or \"<float>\" for a linear-based value. Returns a decibel value."
    [("", Lang.string_t, None, None)]
    Lang.float_t
    (fun p ->
      Lang.float
        (Mm.Audio.dB_of_lin
           (Amplify.parse_db (Lang.to_string (List.assoc "" p)))))
