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

val from_fields :
  ?pos:Pos.t -> ?base_type:Type.t -> Type.t Frame.Fields.t -> Type.t

val make :
  ?pos:Pos.t ->
  ?base_type:Type.t ->
  ?audio:Type.t ->
  ?video:Type.t ->
  ?midi:Type.t ->
  unit ->
  Type.t

val internal : ?pos:Pos.t -> unit -> Type.t
val set_field : Type.t -> Frame.field -> Type.t -> Type.t
val get_field : Type.t -> Frame.field -> Type.t
val content_type : Type.t -> Content.format Frame.Fields.t
