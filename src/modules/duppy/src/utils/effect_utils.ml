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

(* The stdlib's handler re-raises with a plain [raise], which drops the
   backtrace of an exception escaping the computation. *)
let try_with fn arg (handler : 'b Effect.Deep.effect_handler) =
  Effect.Deep.match_with fn arg
    {
      Effect.Deep.retc = Fun.id;
      exnc =
        (fun exn ->
          Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ()));
      effc = handler.effc;
    }
