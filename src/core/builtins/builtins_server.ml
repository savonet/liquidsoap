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

let server_register =
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

let command_t =
  Lang.record_t
    [
      ("name", Lang.string_t);
      ("usage", Lang.string_t);
      ("description", Lang.string_t);
    ]

let _ =
  Lang.add_builtin ~base:Modules.server "commands" ~category:`Interaction
    ~descr:
      "Commands registered with `server.register` or by sources, with their \
       full name (including the namespace), usage and description. Commands \
       added by `server.register.dynamic` are not included."
    [] (Lang.list_t command_t) (fun _ ->
      Lang.list
        (List.map
           (fun { Server.name; usage; descr; _ } ->
             Lang.record
               [
                 ("name", Lang.string name);
                 ("usage", Lang.string usage);
                 ("description", Lang.string descr);
               ])
           (Server.registered_commands ())))

let _ =
  let handler_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t in
  let dynamic_command_t =
    Lang.record_t
      [
        ("name", Lang.string_t);
        ("usage", Lang.string_t);
        ("description", Lang.string_t);
        ("handler", handler_t);
      ]
  in
  Lang.add_builtin ~base:server_register "dynamic" ~category:`Interaction
    ~descr:
      "Register commands computed by a function, called each time a command is \
       executed, described or listed. This lets a set of commands follow \
       changes in the registered commands, see `server.commands`. A command \
       registered with `server.register` takes precedence over a dynamic one \
       of the same name."
    [
      ( "",
        Lang.fun_t [] (Lang.list_t dynamic_command_t),
        None,
        Some
          "Function returning the commands, each with its full name, usage, \
           description and handler." );
    ]
    Lang.unit_t
    (fun p ->
      let f = List.assoc "" p in
      Server.add_dynamic (fun () ->
          List.map
            (fun c ->
              let field name = Value.invoke c name in
              let handler = field "handler" in
              {
                Server.name = Lang.to_string (field "name");
                usage = Lang.to_string (field "usage");
                descr = Lang.to_string (field "description");
                handler =
                  (fun args ->
                    Lang.to_string (Lang.apply handler [("", Lang.string args)]));
              })
            (Lang.to_list (Lang.apply f [])));
      Lang.unit)
