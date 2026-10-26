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

(** [input.harbor.dynamic.regexp] (io.md §1.3, §10), registered when the layer
    is linked: one harbor registration per call, and for each accepted source
    client a single-use source of its own. The client's container is opened and
    described on a scheduler task, the handler is applied to its connection
    record, and the source is fed from a task of its own once an output wakes
    it; a client no output uses within [timeout] is disconnected. The script
    form [input.harbor.dynamic] is defined in the standard library. *)
