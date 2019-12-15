(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(* Support for id3v2 encoding using taglib. *)

exception Found of string

let render m =
  let m = Meta_format.to_metadata m in
  let t = Taglib.Inline.Id3v2.init () in
  let t =
    Taglib.Inline.Id3v2.attach_frame t "TSSE"
      (Printf.sprintf "Encoded by %s" Configure.vendor)
  in
  let f t (l, g) =
    try
      Hashtbl.iter
        (fun k x ->
          if String.uppercase_ascii l = String.uppercase_ascii k then
            raise (Found x))
        m;
      t
    with Found x -> g t x
  in
  let try_int f t x = try f t (int_of_string x) with _ -> t in
  let tags =
    [
      ("title", Taglib.Inline.Id3v2.tag_set_title);
      ("album", Taglib.Inline.Id3v2.tag_set_album);
      ("artist", Taglib.Inline.Id3v2.tag_set_artist);
      ("genre", Taglib.Inline.Id3v2.tag_set_genre);
      ("comment", Taglib.Inline.Id3v2.tag_set_comment);
      ("year", try_int Taglib.Inline.Id3v2.tag_set_year);
      ("tracknumber", try_int Taglib.Inline.Id3v2.tag_set_track);
      ("track", try_int Taglib.Inline.Id3v2.tag_set_track);
    ]
  in
  let t = List.fold_left f t tags in
  Taglib.Inline.Id3v2.render t

let () = Mp3_format.id3v2_export := Some render
