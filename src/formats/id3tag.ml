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

(* $Id: id3tag.ml 2645 2006-07-17 08:28:51Z dbaelde $ *)

type mode = Read_only | Read_write

type file

external open_file : string -> mode -> file = "caml_id3tag_open_file"

external close_file : file -> unit = "caml_id3tag_close_file"

let tag_title = "TIT2"
let tag_artist = "TPE1"
let tag_group = "TPE2"
let tag_composer = "TCOM"
let tag_album = "TALB"
let tag_track = "TRCK"
let tag_year = "TDRC"
let tag_genre = "TCON"
let tag_comment = "COMM"

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
          "Title", tag_title;
          "Artist", tag_artist;
          "Group", tag_group;
          "Composer", tag_composer;
          "Album", tag_album;
          "Track", tag_track;
          "Year", tag_year;
          "Genre", tag_genre;
          "Comment", tag_comment;
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
