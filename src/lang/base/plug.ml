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

(** A plug is something where plug-ins plug. *)

(*
class ['a] plug ?(register_hook = fun _ -> ()) doc insensitive duplicates =
  object
    inherit Doc.item doc
    val mutable plugins : (string * 'a) list = []
    val mutable aliases : (string * 'a) list = []

    method register plugin ?plugin_aliases ?doc ?sdoc v =
      let plugin =
        if insensitive then String.uppercase_ascii plugin else plugin
      in
      let doc () =
        match (doc, sdoc) with
          | Some d, _ -> Lazy.force d
          | None, None -> Doc.trivial "(no doc)"
          | None, Some s -> Doc.trivial s
      in
      if duplicates then (
        subsections <- (plugin, Lazy.from_fun doc) :: subsections;
        plugins <- (plugin, v) :: plugins)
      else (
        subsections <-
          (plugin, Lazy.from_fun doc)
          :: List.filter (fun (k, _) -> k <> plugin) subsections;
        plugins <-
          (plugin, v) :: List.filter (fun (k, _) -> k <> plugin) plugins);
      register_hook (plugin, v);
      match plugin_aliases with
        | Some l -> aliases <- List.map (fun alias -> (alias, v)) l @ aliases
        | None -> ()

    method is_registered a = List.mem_assoc a plugins
    method keys = List.fold_left (fun l (k, _) -> k :: l) [] plugins

    method iter ?(rev = false) f =
      let plugins = if rev then List.rev plugins else plugins in
      List.iter (fun (k, v) -> f k v) plugins

    method get_all = plugins

    method get plugin =
      let plugin =
        if insensitive then String.uppercase_ascii plugin else plugin
      in
      try Some (List.assoc plugin plugins)
      with Not_found -> (
        try Some (List.assoc plugin aliases) with Not_found -> None)
  end
*)

(** A plug. *)
type 'a t = {
  name : string;
  doc : Doc.Plug.t;
  register_hook : string -> 'a -> unit;
  mutable items : (string * 'a) list;
}

(** Create a plug. *)
let create ?(register_hook = fun _ _ -> ()) ~doc name =
  { name; doc = Doc.Plug.create ~doc name; register_hook; items = [] }

let register plug name ~doc value =
  if List.mem_assoc name plug.items then
    failwith ("Plugin already registered in " ^ plug.name ^ ": " ^ name);
  Doc.Plug.add plug.doc ~doc name;
  plug.items <- (name, value) :: plug.items;
  plug.register_hook name value

let get plug name = List.assoc_opt name plug.items

(** List all the plugins. *)
let list plug = plug.items

let iter plug f = List.iter (fun (k, v) -> f k v) plug.items
let find plug f = List.find_opt (fun (k, v) -> f k v) plug.items
