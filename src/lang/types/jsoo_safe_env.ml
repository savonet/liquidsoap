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

open Type

type t = {
  env : (string * scheme) list;
  constraints : (var * string list) list;
      (** Variable records are physically shared with [env]. *)
  unbounded_levels : var list;
      (** Variables at level [max_int], which does not fit in 31 bits. *)
  next_var_name : int;
  next_var_id : int;
}

(* Keeps restored variables clear of those the reading process created before
   loading, while staying within js_of_ocaml's 31-bit integers. *)
let offset = 1 lsl 29

module Physical = Hashtbl.Make (struct
  type t = Type.t

  let equal = ( == )
  let hash = Hashtbl.hash
end)

(* Payloads are hashed structurally and compared physically, as types are: an
   address would not survive the collections a strip triggers. *)
module Payloads = Hashtbl.Make (struct
  type t = Type.custom

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type stripper = {
  types : Type.t Physical.t;
  payloads : int Payloads.t;
  mutable next_payload : int;
  var_maps : (int, var) Hashtbl.t;
  link_maps : (int, var_t) Hashtbl.t;
  mutable stripped_constraints : (var * string list) list;
  mutable unbounded_levels : var list;
  mutable next_var_name : int;
  mutable next_var_id : int;
}

(* What tells the reader which dumped types carried the same payload. *)
let payload_id stripper typ =
  match Payloads.find_opt stripper.payloads typ with
    | Some id -> id
    | None ->
        let id = stripper.next_payload in
        stripper.next_payload <- id + 1;
        Payloads.replace stripper.payloads typ id;
        id

let strip_var stripper (v : var) =
  match Hashtbl.find_opt stripper.var_maps v.name with
    | Some v -> v
    | None ->
        let name = v.name + offset in
        stripper.next_var_name <- max stripper.next_var_name (name + 1);
        let level = if v.level = max_int then 0 else v.level in
        let stripped = { name; level; constraints = Constraints.of_list [] } in
        if v.level = max_int then
          stripper.unbounded_levels <- stripped :: stripper.unbounded_levels;
        let descrs =
          List.map
            (fun c -> c.constr_descr)
            (Constraints.elements v.constraints)
        in
        if descrs <> [] then
          stripper.stripped_constraints <-
            (stripped, descrs) :: stripper.stripped_constraints;
        Hashtbl.replace stripper.var_maps v.name stripped;
        stripped

(* Types are large graphs whose sharing has to survive, as a tree they do not
   fit in memory. *)
let rec strip_type stripper t =
  match Physical.find_opt stripper.types t with
    | Some t -> t
    | None ->
        let stripped = { pos = t.pos; descr = strip_descr stripper t.descr } in
        Physical.replace stripper.types t stripped;
        stripped

and strip_descr stripper descr =
  let map = strip_type stripper in
  match descr with
    | (String | Int | Float | Bool | Never) as descr -> descr
    (* The dispatch table is what cannot be written; a name and a printed
       payload are enough for a reader that implements the type. *)
    | Custom ({ handler_state = Resolved handler } as c) ->
        Custom
          {
            c with
            handler_state =
              Dumped
                {
                  payload_id = payload_id stripper handler.typ;
                  payload = handler.serialize handler.typ;
                };
          }
    | Custom _ as descr -> descr
    | Constr { constructor; params } ->
        Constr
          { constructor; params = List.map (fun (v, t) -> (v, map t)) params }
    | Getter t -> Getter (map t)
    | List { t; json_repr } -> List { t = map t; json_repr }
    | Tuple l -> Tuple (List.map map l)
    | Nullable t -> Nullable (map t)
    | Meth (meth, t) ->
        Meth ({ meth with scheme = strip_scheme stripper meth.scheme }, map t)
    | Arrow (args, t) ->
        Arrow (List.map (fun (b, s, t) -> (b, s, map t)) args, map t)
    | Var { id; contents } -> (
        match Hashtbl.find_opt stripper.link_maps id with
          | Some cell -> Var cell
          | None ->
              let id' = id + offset in
              stripper.next_var_id <- max stripper.next_var_id (id' + 1);
              (* Registered before its contents are mapped so that sharing
                 through links is preserved. *)
              let cell = { id = id'; contents } in
              Hashtbl.replace stripper.link_maps id cell;
              cell.contents <-
                (match contents with
                  | Free v -> Free (strip_var stripper v)
                  | Link (variance, t) -> Link (variance, map t));
              Var cell)

and strip_scheme stripper (vars, t) =
  (List.map (strip_var stripper) vars, strip_type stripper t)

let strip env =
  let stripper =
    {
      types = Physical.create 65536;
      payloads = Payloads.create 64;
      next_payload = 0;
      var_maps = Hashtbl.create 65536;
      link_maps = Hashtbl.create 65536;
      stripped_constraints = [];
      unbounded_levels = [];
      next_var_name = offset;
      next_var_id = offset;
    }
  in
  let env =
    List.map (fun (name, scheme) -> (name, strip_scheme stripper scheme)) env
  in
  {
    env;
    constraints = stripper.stripped_constraints;
    unbounded_levels = stripper.unbounded_levels;
    next_var_name = stripper.next_var_name;
    next_var_id = stripper.next_var_id;
  }

let language_constraints = [record_constr; num_constr; ord_constr]
let bump_counter atom next = if Atomic.get atom < next then Atomic.set atom next

(** What is left of a custom type in a process that does not implement it: a
    name, which only matches itself. Reading a dump is the one place that has to
    settle for it. *)
let opaque_custom_handler name =
  {
    (* Every one of these ignores it. *)
    Type.typ = Obj.magic ();
    serialize = (fun _ -> "");
    copy_with = (fun _ typ -> typ);
    occur_check = (fun _ _ -> ());
    filter_vars = (fun _ vars _ -> vars);
    repr = (fun _ _ _ -> `Constr (name, []));
    subtype = (fun _ _ _ -> ());
    sup = (fun _ typ _ -> typ);
  }

(* Whoever implements the name rebuilds the payload from its printed form;
   anything else stays opaque. A payload is rebuilt once, so types that shared
   one still do. Types share subterms, so each is visited once. *)
let restore_customs env =
  let visited = Physical.create 1024 in
  let restored = Hashtbl.create 16 in
  let handler_of name payload_id payload =
    match Hashtbl.find_opt restored (name, payload_id) with
      | Some handler -> handler
      | None ->
          let handler =
            match Type_custom.of_dump name payload with
              | Some handler -> handler
              | None -> opaque_custom_handler name
          in
          Hashtbl.replace restored (name, payload_id) handler;
          handler
  in
  let rec walk t =
    if not (Physical.mem visited t) then (
      Physical.add visited t t;
      match t.Type.descr with
        | Custom ({ handler_state = Dumped { payload_id; payload } } as c) ->
            c.handler_state <-
              Resolved (handler_of c.custom_name payload_id payload)
        | Custom _ -> ()
        | Constr { params } -> List.iter (fun (_, t) -> walk t) params
        | Getter t | Nullable t -> walk t
        | List { t } -> walk t
        | Tuple l -> List.iter walk l
        | Meth ({ scheme = _, t }, t') ->
            walk t;
            walk t'
        | Arrow (args, t) ->
            List.iter (fun (_, _, t) -> walk t) args;
            walk t
        | Var { contents = Link (_, t) } -> walk t
        | Var { contents = Free _ } | String | Int | Float | Bool | Never -> ())
  in
  List.iter (fun (_, (_, t)) -> walk t) env

let restore { env; constraints; unbounded_levels; next_var_name; next_var_id } =
  restore_customs env;
  List.iter (fun (var : var) -> var.level <- max_int) unbounded_levels;
  List.iter
    (fun ((var : var), descrs) ->
      var.constraints <-
        Constraints.of_list
          (List.filter
             (fun c -> List.mem c.constr_descr descrs)
             language_constraints))
    constraints;
  bump_counter Type_base.var_name_atom next_var_name;
  bump_counter Type_base.var_id_atom next_var_id;
  env

(* What a reader must agree with is the marshaled shape: this module's [t] and
   the type representation it holds. Bump on any change to either, which
   [tests/abi] asks for. *)
let abi_version = 1

let header s =
  match String.index_opt s '\n' with
    | Some header_end -> (
        match String.split_on_char ' ' (String.sub s 0 header_end) with
          | [abi; version] -> Some (int_of_string_opt abi, version, header_end)
          | _ -> None)
    | None -> None

(* Marshal is untyped, so a dump of another shape is rejected from its header,
   before anything is unmarshaled. *)
let to_string ~version (dump : t) =
  Printf.sprintf "%d %s\n%s" abi_version version (Marshal.to_string dump [])

let of_string s : t =
  match header s with
    | Some (Some abi, _, header_end) when abi = abi_version ->
        Marshal.from_string s (header_end + 1)
    | _ ->
        failwith
          (Printf.sprintf
             "This typing environment was not written in format %d." abi_version)

let written_by s =
  match header s with Some (_, version, _) -> Some version | None -> None
