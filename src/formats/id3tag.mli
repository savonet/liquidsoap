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

(**
  * Read id3tags using the libid3tag library.
  *
  * @author Samuel Mimram
  *)

(* $Id: id3tag.mli 4701 2007-10-25 00:55:45Z metamorph68 $ *)

(*
type mode = Read_only | Read_write

type file

val open_file : string -> mode -> file

val close_file : file -> unit

val tag_title : string
val tag_artist : string
val tag_group : string
val tag_composer : string
val tag_album : string
val tag_track : string
val tag_year : string
val tag_genre : string
val tag_comment : string

val get_tag : file -> string -> string
*)

val get_tags : string -> (string * string) list
