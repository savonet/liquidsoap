(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

let log = Dtools.Log.make ["decoder";"taglib"]

(** Configuration keys for taglib. *)
let mime_types =
  Dtools.Conf.list ~p:(Decoder.conf_mime_types#plug "taglib")
    "Mime-types used for decoding metadata using TAGLIB"
    ~d:["audio/mpeg"]

let file_extensions =
  Dtools.Conf.list ~p:(Decoder.conf_file_extensions#plug "taglib")
    "File extensions used for decoding metadata using TAGLIB"
    ~d:["mp3"]

(** We used to force the format. However,
  * now that we check extensions, taglib's
  * automatic format detection should work. *)
let get_tags fname =
  if not (Decoder.test_file ~mimes:mime_types#get 
                            ~extensions:file_extensions#get 
                            ~log fname) then
    raise Not_found ;
  try
    let f = open_file fname in
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

let () = Request.mresolvers#register "TAGLIB" get_tags 
