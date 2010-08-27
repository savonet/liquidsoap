(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

exception Done of Taglib.file_type

(** Force the type for formats
  * we know about.. *)
let taglib_format fname =
  try 
    if Decoder.test_mp3 fname then
      raise (Done Taglib.Mpeg) ;
    (* For now, we force taglib on
     * mp3 only.. *)
    raise Not_found ;
(*    if Decoder.test_mp4 fname then
      raise (Done Taglib.Mp4) ;
    None *)
  with
    | Done x -> Some x

let get_tags fname =
  try
    let file_type = taglib_format fname in
    let f = open_file ?file_type fname in
    Tutils.finalize ~k:(fun () -> close_file f)
    (fun () -> 
      let gt l (n, t) =
        try
          (* Do not pass empty strings.. *)
          match t f with
            | "" -> l 
            | x  -> (n, x) :: l
        with
          | _ -> l
      in
      List.fold_left gt []
        [
          "Title", tag_title;
          "Artist", tag_artist;
          "Album", tag_album;
          "Track", (fun x -> string_of_int (tag_track x));
          "Year", (fun x -> string_of_int (tag_year x));
          "Genre", tag_genre;
          "Comment", tag_comment;
        ])
  with
    | _ -> raise Not_found

let () = Request.mresolvers#register "MP3" get_tags 
