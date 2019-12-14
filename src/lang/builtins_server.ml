(* -*- mode: tuareg; -*- *)
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

open Lang_builtins

let () =
  add_builtin "server.register" ~cat:Interaction
    ~descr:
      "Register a command. You can then execute this function through the \
       server, either telnet or socket."
    [ ("namespace", Lang.string_t, Some (Lang.string ""), None);
      ( "description",
        Lang.string_t,
        Some (Lang.string "No documentation available."),
        Some "A description of your command." );
      ("usage", Lang.string_t, Some (Lang.string ""), None);
      ("", Lang.string_t, None, None);
      ("", Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t, None, None)
    ]
    Lang.unit_t
    (fun p ->
      let namespace = Lang.to_string (List.assoc "namespace" p) in
      let descr = Lang.to_string (List.assoc "description" p) in
      let usage = Lang.to_string (List.assoc "usage" p) in
      let command = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let f x =
        Lang.to_string (Lang.apply ~t:Lang.string_t f [("", Lang.string x)])
      in
      let ns = Pcre.split ~pat:"\\." namespace in
      let usage = if usage = "" then command ^ " <variable>" else usage in
      Server.add ~ns ~usage ~descr command f ;
      Lang.unit)

let () =
  let after_t = Lang.fun_t [] Lang.string_t in
  let wait_t = Lang.fun_t [(false, "", after_t)] Lang.string_t in
  let resume_t = Lang.fun_t [] Lang.unit_t in
  add_builtin "server.condition" ~cat:Interaction
    ~descr:
      "Create a pair of functions `(wait,(signal,broadcast))` used to suspend \
       and resume server command execution. Used to write interactive server \
       commands through `server.wait`, `server.signal`, `server.broadcast` \
       and `server.write`."
    []
    (Lang.product_t wait_t (Lang.product_t resume_t resume_t))
    (fun _ ->
      let opts = Server.condition () in
      let wait =
        Lang.val_fun [("", "", after_t, None)] ~ret_t:Lang.string_t (fun p _ ->
            let after = Lang.to_fun ~t:Lang.string_t (List.assoc "" p) in
            opts.Server.wait (fun () -> Lang.to_string (after [])) ;
            Lang.string "")
      in
      let resume fn =
        Lang.val_fun [] ~ret_t:Lang.unit_t (fun _ _ -> fn () ; Lang.unit)
      in
      Lang.product wait
        (Lang.product
           (resume opts.Server.signal)
           (resume opts.Server.broadcast)))

let () =
  let resume_t = Lang.fun_t [] Lang.string_t in
  let wait_t = Lang.fun_t [(false, "", resume_t)] Lang.string_t in
  let condition_t = Lang.product_t wait_t (Lang.univ_t ()) in
  add_builtin "server.wait" ~cat:Interaction
    ~descr:
      "Wait on a server condition. Used to write interactive server command. \
       Should be used via the syntactic sugar: `server.wait <condition> then \
       <after> end`"
    [ ("", condition_t, None, Some "condition");
      ("", resume_t, None, Some "code to execute when resuming") ]
    Lang.string_t (fun p ->
      let cond = Lang.assoc "" 1 p in
      let wait, _ = Lang.to_product cond in
      let wait = Lang.to_fun ~t:Lang.string_t wait in
      let resume = Lang.assoc "" 2 p in
      wait [("", resume)])

let () =
  let after_t = Lang.fun_t [] Lang.string_t in
  add_builtin "server.write" ~cat:Interaction
    ~descr:
      "Execute a partial write while executing a server command. Should be \
       used via the syntactic sugar: `server.write <string> then <after> end`"
    [ ("", after_t, None, Some "function to run after write");
      ("", Lang.string_t, None, Some "string to write") ] Lang.string_t
    (fun p ->
      let after = Lang.to_fun ~t:Lang.string_t (Lang.assoc "" 1 p) in
      let data = Lang.to_string (Lang.assoc "" 2 p) in
      Server.write ~after:(fun () -> Lang.to_string (after [])) data ;
      Lang.string "")

let () =
  let read ~descr ~args ~mk_marker name =
    let after_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t in
    add_builtin name ~cat:Interaction ~descr
      (("", after_t, None, Some "function to run after write") :: args)
      Lang.string_t (fun p ->
        let marker = mk_marker p in
        let after = Lang.to_fun ~t:Lang.string_t (Lang.assoc "" 1 p) in
        Server.read
          ~after:(fun ret -> Lang.to_string (after [("", Lang.string ret)]))
          marker ;
        Lang.string "")
  in
  read
    ~args:[("", Lang.string_t, None, Some "Read marker")]
    ~mk_marker:(fun p -> Duppy.Io.Split (Lang.to_string (Lang.assoc "" 2 p)))
    ~descr:
      "Read a string from the client up-to a marker. Marker can be any string \
       of regular expression. Should be used via the syntactic sugar: \
       `server.read <marker> : <varname> then <after> end`"
    "server.read" ;
  read
    ~args:[("", Lang.int_t, None, Some "Number of characters to read")]
    ~mk_marker:(fun p -> Duppy.Io.Length (Lang.to_int (Lang.assoc "" 2 p)))
    ~descr:
      "Read a string of fixed length from the client up-to a marker. Should \
       be used via the syntactic sugar: `server.readchars <len> : <varname> \
       then <after> end`"
    "server.readchars" ;
  read ~args:[]
    ~mk_marker:(fun _ -> Duppy.Io.Split "[\r\n]+")
    ~descr:
      "Read a line from the client. Should be used via the syntactic sugar: \
       `server.readline <varname> then <after> end`"
    "server.readline"
