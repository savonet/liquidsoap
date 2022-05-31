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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Operations on frames, which are small portions of streams. *)

(** {2 Lang_frame definitions} *)

type 'a fields = { audio : 'a; video : 'a; midi : 'a }

(** High-level description of the content. *)
type kind =
  [ `Any
  | `Internal
  | `Kind of Lang_content.kind
  | `Format of Lang_content.format ]

val none : kind

type content_kind = kind fields

(** Precise description of the channel types for the current track. *)
type content_type = Lang_content.format fields

type content = Lang_content.data fields

(** Metadata of a frame. *)
type metadata = (string, string) Hashtbl.t
