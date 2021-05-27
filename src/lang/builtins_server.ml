(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

open Lang_builtins

let () =
  add_builtin "server.register" ~cat:Interaction
    ~descr:
      "Register a command. You can then execute this function through the \
       server, either telnet or socket."
    [
      ( "namespace",
        Lang.string_t,
        Some (Lang.string ""),
        Some
          "Used to group multiple commands for the same functionality. If \
           sepecified, the command will be named `namespace.command`." );
      ( "description",
        Lang.string_t,
        Some (Lang.string "No documentation available."),
        Some "A description of your command." );
      ( "usage",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Description of how the command should be used." );
      ("", Lang.string_t, None, Some "Name of the command.");
      ( "",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t,
        None,
        Some
          "Function called when the command is executed. It takes as argument \
           the argument passed on the commandline and returns the message \
           which will be printed on the commandline." );
    ]
    Lang.unit_t
    (fun p ->
      let namespace = Lang.to_string (List.assoc "namespace" p) in
      let descr = Lang.to_string (List.assoc "description" p) in
      let usage = Lang.to_string (List.assoc "usage" p) in
      let command = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let f x = Lang.to_string (Lang.apply f [("", Lang.string x)]) in
      let ns = Pcre.split ~pat:"\\." namespace in
      let usage = if usage = "" then command ^ " <variable>" else usage in
      Server.add ~ns ~usage ~descr command f;
      Lang.unit)
