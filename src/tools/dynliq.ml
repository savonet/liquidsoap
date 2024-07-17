(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

let () = Dynlink.init ()

let print_linking_error =
  function
  | Dynlink.Undefined_global a ->
      Printf.sprintf "Undefined_global %s" a
  | Dynlink.Unavailable_primitive a ->
      Printf.sprintf "Unavailable_primitive %s" a
  | Dynlink.Uninitialized_global a ->
      Printf.sprintf "Uninitialized_global %s" a

let print_error =
  function
  | Dynlink.Unsafe_file -> Printf.printf "unsafe\n"
  | Dynlink.File_not_found _ -> Printf.printf "not found\n"
  | Dynlink.Inconsistent_import _ -> Printf.printf "inconsistent\n"
  | Dynlink.Not_a_bytecode_file _ -> Printf.printf "nobyte\n"
  | Dynlink.Unavailable_unit _ -> Printf.printf "unavailable\n"
  | Dynlink.Corrupted_interface _ -> Printf.printf "corrupted\n"
  | Dynlink.Linking_error (s,e) ->
      Printf.printf "linking %s\n" (print_linking_error e)
  | _ -> Printf.printf "???\n"

let load filename =
  try Dynlink.loadfile filename with
  | Dynlink.Error e -> print_error e
