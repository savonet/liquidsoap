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

let _ =
  Lang.add_builtin ~base:Modules.server "register" ~category:`Interaction
    ~descr:
      "Register a command. You can then execute this function through the \
       server, either telnet or socket."
    [
      ( "namespace",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Used to group multiple commands for the same functionality. If \
           specified, the command will be named `namespace.command`." );
      ( "description",
        Lang.string_t,
        Some (Lang.string "No documentation available."),
        Some "A description of your command." );
      ( "usage",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
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
      let ns =
        match
          Lang.to_valued_option Lang.to_string (List.assoc "namespace" p)
        with
          | None -> []
          | Some s -> String.split_on_char '.' s
      in
      let descr = Lang.to_string (List.assoc "description" p) in
      let command = Lang.to_string (Lang.assoc "" 1 p) in
      let usage =
        Option.value ~default:command
          (Option.map Lang.to_string (Lang.to_option (List.assoc "usage" p)))
      in
      let f = Lang.assoc "" 2 p in
      let f x = Lang.to_string (Lang.apply f [("", Lang.string x)]) in
      Server.add ~ns ~usage ~descr command f;
      Lang.unit)
