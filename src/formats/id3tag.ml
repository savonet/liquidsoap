(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(* @author Samuel Mimram *)

(* $Id: id3tag.ml 2898 2007-03-02 12:26:42Z dbaelde $ *)

type mode = Read_only | Read_write

type file

external open_file : string -> mode -> file = "caml_id3tag_open_file"

external close_file : file -> unit = "caml_id3tag_close_file"

external get_tag : file -> string -> string = "caml_id3tag_get_tag"

let get_tags fname =
  try
    let f = open_file fname Read_only in
    let ans = ref [] in
    let gt (n, t) =
      try
        ans := (n, get_tag f t)::!ans
      with
        | _ -> ()
    in
      List.iter gt
        [
          "Title", "TIT2";
          "Artist", "TPE1";
          "Group", "TPE2";
          "Composer", "TCOM";
          "Album", "TALB";
          "Track", "TRCK";
          "Year", "TDRC";
          "Genre", "TCON";
          "Comment", "COMM";
          "Copyright", "WCOP";
          "File webpage", "WOAF";
          "Artist webpage", "WOAR";
          "Source webpage", "WOAS";
          "Radio webpage", "WORS";
          "Publishers webpage", "WPUB";
          "User URL", "WXXX"
        ];
      close_file f;
      !ans
  with
    | _ -> []
