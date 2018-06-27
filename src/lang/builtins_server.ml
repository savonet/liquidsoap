(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2018 Savonet team

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

open Lang_builtins

let () =
  add_builtin "server.register" ~cat:Interaction
    ~descr:"Register a command. You can then execute this function \
            through the server, either telnet or socket."
    [ "namespace",Lang.string_t,Some (Lang.string ""),None ;
      "description",Lang.string_t,
      Some (Lang.string "No documentation available."),
      Some "A description of your command." ;
      "usage",Lang.string_t,Some (Lang.string ""),None ;
      "",Lang.string_t,None,None ;
      "",Lang.fun_t [false,"",Lang.string_t] Lang.string_t,None,None ]
    Lang.unit_t
    (fun p ->
       let namespace = Lang.to_string (List.assoc "namespace" p) in
       let descr = Lang.to_string (List.assoc "description" p) in
       let usage = Lang.to_string (List.assoc "usage" p) in
       let command = Lang.to_string (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
       let f x =
         Lang.to_string (Lang.apply ~t:Lang.string_t f ["",Lang.string x])
       in
       let ns = (Pcre.split ~pat:"\\." namespace) in
       let usage = if usage = "" then command ^ " <variable>" else usage in
           Server.add ~ns ~usage ~descr command f ;
         Lang.unit)

let () =
  let after_t =
    Lang.fun_t [] Lang.string_t
  in
  let wait_t =
    Lang.fun_t [false,"",after_t] Lang.string_t
  in
  let resume_t =
    Lang.fun_t [] Lang.unit_t
  in
  add_builtin "server.wait" ~cat:Interaction
    ~descr:"Create a pair of functions @(wait,resume)@ used to suspend and resume \
            server command execution. Useful in conjonction with @server.partial_write()@ \
            to write interactive commands."
    []
    (Lang.product_t wait_t resume_t)
    (fun _ ->
      let opts = Server.condition () in
      let wait = Lang.val_fun
        ["", "", after_t, None] ~ret_t:Lang.string_t
        (fun p _ ->
          let resume =
            Lang.to_fun ~t:Lang.unit_t (List.assoc "" p)
          in
          opts.Server.wait (fun () ->
            Lang.to_string (resume []));
          Lang.string "")
      in
      let resume = Lang.val_fun [] ~ret_t:Lang.unit_t
        (fun _ _ ->
          opts.Server.signal ();
          Lang.unit)
      in
      Lang.product wait resume)

let () =
  let after_t =
    Lang.fun_t [] Lang.string_t
  in
  add_builtin "server.partial_write" ~cat:Interaction
    ~descr:"Execute a partial write while executing a server command."
    ["after",after_t,None,Some "function to run after write";
     "",Lang.string_t,None,Some "string to write"]
    Lang.string_t
    (fun p ->
       let data = Lang.to_string (List.assoc "" p) in
       let after =
         Lang.to_fun ~t:Lang.string_t (List.assoc "after" p)
       in
       Server.partial_write ~after:(fun () ->
         Lang.to_string (after [])) data;
       Lang.string "")
