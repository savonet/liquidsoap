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

(** Register a flag for a string identified by its content digest. The map entry
    is kept alive as long as [s] is live. Multiple registrations for strings
    with the same content accumulate their flags. *)
val register : string -> Flags.flag -> unit

(** Merge the flags stored for [s]'s content digest into [flags] and return the
    result. If an entry exists, also anchors it to [s] via a finalizer so the
    entry stays alive as long as [s] is live. *)
val merge_flags : string -> Flags.flags -> Flags.flags
