(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

module type Spec = sig
  val name : string
end

module type Custom = sig
  type Type_base.custom += Type

  val descr : Type_base.descr
  val is_descr : Type_base.descr -> bool
end

module Make (_ : Spec) : Custom
module Int : Custom

val int : Type_base.descr

module Float : Custom

val float : Type_base.descr

module String : Custom

val string : Type_base.descr

module Bool : Custom

val bool : Type_base.descr

module Never : Custom

val never : Type_base.descr
val is_ground : Type_base.custom -> bool
