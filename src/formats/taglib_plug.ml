(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

open Taglib

let get_tags ~format fname =
  try
    (* Disable other formats for now... *)
    if (format <> "MP3") then
      raise Not_found ;

    let f = open_file ~file_type:Mpeg fname in
    let ans = ref [] in
    let gt (n, t) =
      try
        ans := (n, t f)::!ans
      with
        | _ -> ()
    in
      List.iter gt
        [
          "Title", tag_title;
          "Artist", tag_artist;
          "Album", tag_album;
          "Track", (fun x -> string_of_int (tag_track x));
          "Year", (fun x -> string_of_int (tag_year x));
          "Genre", tag_genre;
          "Comment", tag_comment;
        ];
      close_file f;
      !ans
  with
    | _ -> []

let () = Request.mresolvers#register "MP3" get_tags 
