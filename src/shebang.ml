(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(* Equality of basenames, as a weak form of path equivalence. *)
let ( === ) a b = Filename.basename a = Filename.basename b

(** Re-parse the command line to handle #! calls. *)
let argv =
  if
    (* The env variable "_" contains the name under which we've been invoked.
     * When liquidsoap is invoked via the shebang line of a script,
     * the invokation name is that liq script.
     * When liquidsoap is invoked via "liquidsoap", the invokation name is the
     * full path to the binary. Doing the check against $0 rather than directly
     * "liquidsoap" avoids problems if the user decides to change the name or
     * use a symlink to liquidsoap.
     * Aliases do not seem to have any effect on that system. *)
    try
      (* Normal invokation. *)
      Sys.argv.(0) === Sys.getenv "_"
      || (* Invokation from gdb/strace/valgrind...
          * When liquidsoap is invoked through gdb, env[_] is "../gdb".
          * For a real #! invokation, env[_] (the script name) should be
          * found on the command-line, either at position 1 or 2. *)
         not
           ( (Array.length Sys.argv > 1 && Sys.getenv "_" === Sys.argv.(1))
           || (Array.length Sys.argv > 2 && Sys.getenv "_" === Sys.argv.(2)) )
    with Not_found ->
      (* In case ENV[_] is not defined, for compatibility: *)
      true
  then Sys.argv
  else (
    (* Liquidsoap has been invoked using a #!. We have:
     *   Sys.argv = $0 "opt0 .. optN" script.liq argv3 .. argvN
     * We build:
     *       argv = $0 opt0 .. optN script.liq -- argv3 .. argvN
     * There may or may not be a list of options "opt0 .. optN" on the shebang
     * line, in which case the second parameter will be the script name.
     * Currently I don't implement a full parsing of opts (quotations and
     * escapings are missing) but that should do for a long time. *)
    let opts, script, more =
      if Sys.argv.(1) === Sys.getenv "_" then
        ( [||],
          [|Sys.argv.(1)|],
          Array.sub Sys.argv 2 (Array.length Sys.argv - 2) )
      else
        ( Array.of_list (Pcre.split ~pat:"\\s+" Sys.argv.(1)),
          [|Sys.argv.(2)|],
          Array.sub Sys.argv 3 (Array.length Sys.argv - 3) )
    in
    Array.concat [[|Sys.argv.(0)|]; opts; script; [|"--"|]; more] )
