(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Liquidsoap_lang

val make :
  ?pos:Pos.t -> audio:Type.t -> video:Type.t -> midi:Type.t -> unit -> Type.t

val univ : ?pos:Pos.t -> unit -> Type.t

val make_kind :
  ?pos:Pos.t ->
  [< `Any
  | `Format of Content_base.format
  | `Internal
  | `Kind of Content_base.kind ] ->
  Type.t

val set_audio : Type.t -> Type.t -> Type.t
val set_video : Type.t -> Type.t -> Type.t
val set_midi : Type.t -> Type.t -> Type.t
val get_audio : Type.t -> Type.t
val get_video : Type.t -> Type.t
val get_midi : Type.t -> Type.t
val to_string : Type.t -> string
val content_type : Type.t -> Content_base.format Frame.Fields.t
