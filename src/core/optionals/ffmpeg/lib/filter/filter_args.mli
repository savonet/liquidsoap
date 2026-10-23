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

(** The arguments a filter builtin takes from the filter's AVOptions (filters.md
    §5): one per distinct option name, typed, documented and rendered into an
    argument pair. An argument's [render] converts, checks and renders a
    non-null value, raising [Error.Invalid_value] on a value the option rejects.
*)

type arg = {
  name : string;
  typ : Lang.t;
  doc : string option;
  render : Lang.value -> Avfilter.args;
}

val of_filter : [ `Unattached ] Avfilter.filter -> arg list

(** The pairs of the non-null arguments in [values], the builtin's environment.
*)
val render : arg list -> (string * Lang.value) list -> Avfilter.args list
