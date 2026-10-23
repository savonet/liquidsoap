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

(** The FFmpeg media decoder, duration resolver and metadata resolver
    (decoder.md §1, §3, §5, §10.4, §11.1), registered when the layer is linked.
    [file_type] is the probe of §5; [tags] the resolver's tags of §10.4, before
    the request layer lowercases them. *)

val file_type :
  metadata:Frame.metadata ->
  ctype:Frame.content_type ->
  string ->
  Frame.content_type option

val tags : metadata:Frame.metadata -> string -> (string * string) list
