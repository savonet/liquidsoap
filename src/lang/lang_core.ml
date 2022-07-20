(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

include Value
module Ground = Term.Ground
open Ground

type t = Type.t
type scheme = Type.scheme
type value = Value.t = { pos : Pos.Option.t; value : in_value }

(** Type construction *)

let ground_t x = Type.make (Type.Ground x)
let int_t = ground_t Type.Ground.Int
let unit_t = Type.make Type.unit
let float_t = ground_t Type.Ground.Float
let bool_t = ground_t Type.Ground.Bool
let string_t = ground_t Type.Ground.String
let tuple_t l = Type.make (Type.Tuple l)
let product_t a b = tuple_t [a; b]

let rec record_t = function
  | [] -> unit_t
  | (l, t) :: r -> Type.meth l ([], t) (record_t r)

let rec method_t t0 = function
  | [] -> t0
  | (l, t, doc) :: r -> Type.meth l t ~doc (method_t t0 r)

let of_tuple_t t =
  match (Type.deref t).Type.descr with Type.Tuple l -> l | _ -> assert false

let of_product_t t =
  match of_tuple_t t with [a; b] -> (a, b) | _ -> assert false

let fun_t p b = Type.make (Type.Arrow (p, b))
let list_t t = Type.make Type.(List { t; json_repr = `Tuple })

let of_list_t t =
  match (Type.deref t).Type.descr with
    | Type.(List { t }) -> t
    | _ -> assert false

let nullable_t t = Type.make (Type.Nullable t)
let ref_t t = Term.ref_t t
let metadata_t = list_t (product_t string_t string_t)
let univ_t ?(constraints = []) () = Type.var ~constraints ()
let getter_t a = Type.make (Type.Getter a)
let source_t t = Term.source_t t
let of_source_t t = Term.of_source_t t
let format_t t = Term.format_t t
let kind_t = Content_unifier.make_content
let of_kind_t = Content_unifier.content
let kind_none_t = kind_t Frame.none

let empty =
  Frame.mk_fields ~audio:Frame.none ~video:Frame.none ~midi:Frame.none ()

let any = Frame.mk_fields ~audio:`Any ~video:`Any ~midi:`Any ()

let internal =
  Frame.mk_fields ~audio:`Internal ~video:`Internal ~midi:`Internal ()

let fields_t fields = Content_unifier.make ~fields ~sealed:true ()
let of_fields_t = Content_unifier.fields

let content_t fields =
  let fields = Frame.Fields.map Content_unifier.make_content fields in
  Content_unifier.make ~fields ~sealed:true ()

let of_content_t k =
  Frame.Fields.map Content_unifier.content (Content_unifier.fields k)

let set_field_t fields field v =
  Content_unifier.make
    ~fields:(Frame.Fields.add field v (Content_unifier.fields fields))
    ~sealed:(Content_unifier.sealed fields)
    ()

let get_field_t fields field =
  Frame.Fields.find field (Content_unifier.fields fields)

(** Value construction *)

let mk ?pos value = { pos; value }
let unit = mk unit
let int i = mk (Ground (Int i))
let bool i = mk (Ground (Bool i))
let float i = mk (Ground (Float i))
let string i = mk (Ground (String i))
let tuple l = mk (Tuple l)
let product a b = tuple [a; b]
let list l = mk (List l)
let null = mk Null

let rec meth v0 = function
  | [] -> v0
  | (l, v) :: r -> mk (Meth (l, v, meth v0 r))

let record = meth unit
let reference x = mk (Ref x)
let val_fun p f = mk (FFI (p, f))

let val_cst_fun p c =
  let p = List.map (fun (l, d) -> (l, "_", d)) p in
  let f t tm = mk (Fun (p, [], { Term.t; Term.term = tm })) in
  let mkg t = Type.make (Type.Ground t) in
  (* Convert the value into a term if possible, to enable introspection, mostly
     for printing. *)
  match c.value with
    | Tuple [] -> f (Type.make Type.unit) Term.unit
    | Ground (Int i) ->
        f (mkg Type.Ground.Int) (Term.Ground (Term.Ground.Int i))
    | Ground (Bool i) ->
        f (mkg Type.Ground.Bool) (Term.Ground (Term.Ground.Bool i))
    | Ground (Float i) ->
        f (mkg Type.Ground.Float) (Term.Ground (Term.Ground.Float i))
    | Ground (String i) ->
        f (mkg Type.Ground.String) (Term.Ground (Term.Ground.String i))
    | _ -> mk (FFI (p, fun _ -> c))

let metadata m =
  list (Hashtbl.fold (fun k v l -> product (string k) (string v) :: l) m [])

(** Helpers for defining builtin functions. *)

type proto = (string * t * value option * string option) list

let doc_of_prototype_item ~generalized t d doc =
  let doc = match doc with None -> "(no doc)" | Some d -> d in
  let item = new Doc.item doc in
  item#add_subsection "type"
    (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
  item#add_subsection "default"
    (match d with
      | None -> Lazy.from_val (Doc.trivial "None")
      | Some d -> Lazy.from_fun (fun () -> Doc.trivial (Value.to_string d)));
  item

let builtin_type p t =
  Type.make
    (Type.Arrow (List.map (fun (lbl, t, opt, _) -> (opt <> None, lbl, t)) p, t))

let to_plugin_doc category flags examples main_doc proto return_t =
  let item = new Doc.item ~sort:false main_doc in
  let meths, return_t = Type.split_meths return_t in
  let t = builtin_type proto return_t in
  let generalized = Type.filter_vars (fun _ -> true) t in
  item#add_subsection "_category"
    (Lazy.from_fun (fun () ->
         Doc.trivial (Documentation.string_of_category category)));
  item#add_subsection "_type"
    (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
  List.iter
    (fun f ->
      item#add_subsection "_flag"
        (Lazy.from_fun (fun () -> Doc.trivial (Documentation.string_of_flag f))))
    flags;
  if meths <> [] then
    item#add_subsection "_methods"
      (Lazy.from_fun (fun () -> Repr.doc_of_meths meths));
  List.iter
    (fun e ->
      let e =
        if e.[0] = '\n' then String.sub e 1 (String.length e - 1) else e
      in
      let e =
        if e.[String.length e - 1] = '\n' then
          String.sub e 0 (String.length e - 1)
        else e
      in
      item#add_subsection "_example" (Lazy.from_fun (fun () -> new Doc.item e)))
    examples;
  List.iter
    (fun (l, t, d, doc) ->
      item#add_subsection
        (if l = "" then "(unlabeled)" else l)
        (Lazy.from_fun (fun () -> doc_of_prototype_item ~generalized t d doc)))
    proto;
  item

let meth_fun = meth

let add_builtin ~category ~descr ?(flags = []) ?(meth = []) ?(examples = [])
    name proto return_t f =
  let return_t =
    if meth = [] then return_t
    else (
      let meth = List.map (fun (l, t, d, _) -> (l, t, d)) meth in
      method_t return_t meth)
  in
  let f =
    if meth = [] then f
    else (
      let meth = List.map (fun (l, _, _, f) -> (l, f)) meth in
      fun p -> meth_fun (f p) meth)
  in
  let t = builtin_type proto return_t in
  let value =
    {
      pos = None;
      value = FFI (List.map (fun (lbl, _, opt, _) -> (lbl, lbl, opt)) proto, f);
    }
  in
  let generalized = Type.filter_vars (fun _ -> true) t in
  let doc () = to_plugin_doc category flags examples descr proto return_t in
  let doc = Lazy.from_fun doc in
  Environment.add_builtin ~doc
    (String.split_on_char '.' name)
    ((generalized, t), value)

let add_builtin_base ~category ~descr ?(flags = []) name value t =
  let value = { pos = t.Type.pos; value } in
  let generalized = Type.filter_vars (fun _ -> true) t in
  let doc () =
    let doc = new Doc.item ~sort:false descr in
    doc#add_subsection "_category"
      (Lazy.from_fun (fun () ->
           Doc.trivial (Documentation.string_of_category category)));
    doc#add_subsection "_type"
      (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
    List.iter
      (fun f ->
        doc#add_subsection "_flag"
          (Lazy.from_fun (fun () ->
               Doc.trivial (Documentation.string_of_flag f))))
      flags;
    doc
  in
  Environment.add_builtin ~doc:(Lazy.from_fun doc)
    (String.split_on_char '.' name)
    ((generalized, t), value)

let add_module name = Environment.add_module (String.split_on_char '.' name)

(* Delay this function in order not to have Lang depend on Evaluation. *)
let apply_fun : (?pos:Pos.t -> value -> env -> value) ref =
  ref (fun ?pos:_ _ -> assert false)

let apply f p = !Hooks.collect_after (fun () -> !apply_fun f p)

(** {1 High-level manipulation of values} *)

let to_unit t = match (demeth t).value with Tuple [] -> () | _ -> assert false

let to_bool t =
  match (demeth t).value with Ground (Bool b) -> b | _ -> assert false

let to_bool_getter t =
  match (demeth t).value with
    | Ground (Bool b) -> fun () -> b
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply t []).value with
            | Ground (Bool b) -> b
            | _ -> assert false)
    | _ -> assert false

let to_fun f =
  match (demeth f).value with
    | Fun _ | FFI _ -> fun args -> apply f args
    | _ -> assert false

let to_string t =
  match (demeth t).value with Ground (String s) -> s | _ -> assert false

let to_string_getter t =
  match (demeth t).value with
    | Ground (String s) -> fun () -> s
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply t []).value with
            | Ground (String s) -> s
            | _ -> assert false)
    | _ -> assert false

let to_float t =
  match (demeth t).value with Ground (Float s) -> s | _ -> assert false

let to_float_getter t =
  match (demeth t).value with
    | Ground (Float s) -> fun () -> s
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply t []).value with
            | Ground (Float s) -> s
            | _ -> assert false)
    | _ -> assert false

let to_int t =
  match (demeth t).value with Ground (Int s) -> s | _ -> assert false

let to_int_getter t =
  match (demeth t).value with
    | Ground (Int n) -> fun () -> n
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply t []).value with
            | Ground (Int n) -> n
            | _ -> assert false)
    | _ -> assert false

let to_num t =
  match (demeth t).value with
    | Ground (Int n) -> `Int n
    | Ground (Float x) -> `Float x
    | _ -> assert false

let to_list t = match (demeth t).value with List l -> l | _ -> assert false
let to_tuple t = match (demeth t).value with Tuple l -> l | _ -> assert false
let to_option t = match (demeth t).value with Null -> None | _ -> Some t
let to_valued_option convert v = Option.map convert (to_option v)

let to_default_option ~default convert v =
  Option.value ~default (to_valued_option convert v)

let to_product t =
  match (demeth t).value with Tuple [a; b] -> (a, b) | _ -> assert false

let to_ref t = match t.value with Ref r -> r | _ -> assert false

let to_metadata_list t =
  let pop v =
    let f (a, b) = (to_string a, to_string b) in
    f (to_product v)
  in
  List.map pop (to_list t)

let to_metadata t =
  let t = to_metadata_list t in
  let metas = Hashtbl.create 10 in
  List.iter (fun (a, b) -> Hashtbl.add metas a b) t;
  metas

let to_string_list l = List.map to_string (to_list l)
let to_int_list l = List.map to_int (to_list l)

let to_getter t =
  match (demeth t).value with
    | Fun ([], _, _) | FFI ([], _) -> fun () -> apply t []
    | _ -> fun () -> t

(** [assoc lbl n l] returns the [n]th element in [l]
  * of which the first component is [lbl]. *)
let rec assoc label n = function
  | [] -> raise Not_found
  | (l, e) :: tl ->
      if l = label then if n = 1 then e else assoc label (n - 1) tl
      else assoc label n tl

let raise_error = Runtime_error.error

let raise_as_runtime ~bt ~kind exn =
  match exn with
    | Term.Runtime_error _ -> Printexc.raise_with_backtrace exn bt
    | exn ->
        raise_error ~bt
          ~message:
            (Printf.sprintf "%s\nBacktrace:\n%s" (Printexc.to_string exn)
               (Printexc.raw_backtrace_to_string bt))
          kind

let environment () =
  let l = Unix.environment () in
  (* Split at first occurrence of '='. Return v,"" if
   * no '=' could be found. *)
  let split s =
    try
      let pos = String.index s '=' in
      (String.sub s 0 pos, String.sub s (pos + 1) (String.length s - pos - 1))
    with _ -> (s, "")
  in
  let l = Array.to_list l in
  List.map split l
