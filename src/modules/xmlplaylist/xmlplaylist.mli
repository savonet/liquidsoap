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

(** Generic xml playlist parsing module

    This module implements basic playlist parsing for various xml formats.

    Currently supported formats are: [podcast (rss), xspf, smil asx] *)

(** {2 Types and exceptions} *)

type error = XmlError of string | Empty | UnknownType | Internal

exception Error of error

type format = Podcast | Xspf | Smil | Asx

(** {2 Functions} *)

(** Get meaning of Error e *)
val string_of_error : error -> string

(** [tracks data] performs whole process and outputs a list of metadatas,uri
    from given xml data string

    All metadatas are what is provided by the playlist The only variable name
    that is changed is the author because each formats has its own field for
    that. The module will use "artist".

    Order of tracks is preserved.

    [format] is an optional argument, used to force format detection. Otherwise.
    [detect_format] is used. *)
val tracks : ?format:format -> string -> ((string * string) list * string) list

(** Try to detect the format automatically. *)
val detect_format : string -> format
