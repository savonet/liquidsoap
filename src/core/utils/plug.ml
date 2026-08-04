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

let log = Log.make ["plug"]

exception Incomplete of string

type 'a t = {
  name : string;
  doc : Doc.Plug.t;
  ordered_by : string list Dtools.Conf.t option;
  mutable on_complete : ((string * 'a) list -> unit) list;
  mutable items : (string * 'a) list;
}

(* Every plug becomes complete at the same moment: when Lifecycle.load runs,
   every OCaml module having had its chance to register. Scripts may still add
   entries after that -- add_protocol and add_playlist_parser are exposed to
   the language -- so this is not a seal, only the point from which asking what
   a plug contains is a question about the program rather than about the
   linker. *)
let is_complete = ref false

(* Completing has to run every plug's callbacks, and plugs are created by
   modules this one cannot see, so each adds itself here as it is created. *)
let complete_actions = Queue.create ()

let create ?ordered_by ~doc name =
  let plug =
    {
      name;
      doc = Doc.Plug.create ~doc name;
      ordered_by;
      on_complete = [];
      items = [];
    }
  in
  Queue.add
    (fun () -> List.iter (fun fn -> fn (List.rev plug.items)) plug.on_complete)
    complete_actions;
  plug

let register plug name ~doc value =
  if List.mem_assoc name plug.items then
    failwith ("Plugin already registered in " ^ plug.name ^ ": " ^ name);
  Doc.Plug.add plug.doc ~doc name;
  plug.items <- (name, value) :: plug.items;
  (* The settings list doubles as the order the entries are tried in: it is
     appended to as they register, and the user can reorder it afterwards. *)
    match plug.ordered_by with
    | None -> ()
    | Some conf -> (
        match conf#get_d with
          | None -> conf#set_d (Some [name])
          | Some d -> conf#set_d (Some (d @ [name])))

(* Looking one entry up by name does not depend on who else has registered, so
   it is allowed at any time. Enumerating is not. *)
let get plug name = List.assoc_opt name plug.items

let entries plug =
  if not !is_complete then raise (Incomplete plug.name);
  List.rev plug.items

let iter plug f = List.iter (fun (k, v) -> f k v) (entries plug)
let find plug f = List.find_opt (fun (k, v) -> f k v) (entries plug)

(* Entries in the order the user configured, dropping any that no longer exist
   and saying so. Only meaningful for plugs created with [ordered_by]. *)
let ordered_entries plug =
  let items = entries plug in
  match plug.ordered_by with
    | None -> items
    | Some conf ->
        List.filter_map
          (fun name ->
            match List.assoc_opt name items with
              | Some v -> Some (name, v)
              | None ->
                  log#severe "Cannot find %s in %s!" name plug.name;
                  None)
          conf#get

let on_complete plug fn =
  if !is_complete then fn (List.rev plug.items)
  else plug.on_complete <- plug.on_complete @ [fn]

let mark_complete () =
  if not !is_complete then (
    is_complete := true;
    Queue.iter (fun fn -> fn ()) complete_actions)
