(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
type module_name = string
type scheme = Type.scheme
type value = Value.t = { pos : Pos.Option.t; value : in_value }

(** Type construction *)

let int_t = Type.make Type.Ground.int
let unit_t = Type.make Type.unit
let float_t = Type.make Type.Ground.float
let bool_t = Type.make Type.Ground.bool
let string_t = Type.make Type.Ground.string
let tuple_t l = Type.make (Type.Tuple l)
let product_t a b = tuple_t [a; b]

let rec record_t = function
  | [] -> unit_t
  | (l, t) :: r -> Type.meth l ([], t) (record_t r)

let rec optional_record_t = function
  | [] -> unit_t
  | (l, t) :: r -> Type.meth ~optional:true l ([], t) (optional_record_t r)

let rec method_t t0 = function
  | [] -> t0
  | (l, t, doc) :: r -> Type.meth l t ~doc (method_t t0 r)

let rec optional_method_t t0 = function
  | [] -> t0
  | (l, t, doc) :: r ->
      Type.meth l t ~doc ~optional:true (optional_method_t t0 r)

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
let univ_t ?(constraints = []) () = Type.var ~constraints ()
let getter_t a = Type.make (Type.Getter a)

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
  let mkg g = Type.make g in
  (* Convert the value into a term if possible, to enable introspection, mostly
     for printing. *)
  match c.value with
    | Tuple [] -> f (Type.make Type.unit) Term.unit
    | Ground (Int i) ->
        f (mkg Type.Ground.int) (Term.Ground (Term.Ground.Int i))
    | Ground (Bool i) ->
        f (mkg Type.Ground.bool) (Term.Ground (Term.Ground.Bool i))
    | Ground (Float i) ->
        f (mkg Type.Ground.float) (Term.Ground (Term.Ground.Float i))
    | Ground (String i) ->
        f (mkg Type.Ground.string) (Term.Ground (Term.Ground.String i))
    | _ -> mk (FFI (p, fun _ -> c))

(** Helpers for defining builtin functions. *)

type proto = (string * t * value option * string option) list

let builtin_type p t =
  Type.make
    (Type.Arrow (List.map (fun (lbl, t, opt, _) -> (opt <> None, lbl, t)) p, t))

let meth_fun = meth

let mk_module_name ?base name =
  if String.index_opt name '.' <> None then
    failwith ("module name " ^ name ^ " has a dot in it!");
  match base with None -> name | Some b -> b ^ "." ^ name

let add_builtin ~category ~descr ?(flags = []) ?(meth = []) ?(examples = [])
    ?base name proto return_t f =
  let name = mk_module_name ?base name in
  let return_t =
    let meth = List.map (fun (l, t, d, _) -> (l, t, d)) meth in
    method_t return_t meth
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
  let doc () =
    let meth, return_t = Type.split_meths return_t in
    let t = builtin_type proto return_t in
    let generalized = Typing.filter_vars (fun _ -> true) t in
    let examples =
      List.map
        (fun e ->
          (* Remove leading and trailing newline *)
          let e =
            if e.[0] = '\n' then String.sub e 1 (String.length e - 1) else e
          in
          let e =
            if e.[String.length e - 1] = '\n' then
              String.sub e 0 (String.length e - 1)
            else e
          in
          e)
        examples
    in
    let arguments =
      List.map
        (fun (l, t, d, doc) ->
          ( (if l = "" then None else Some l),
            Doc.Value.
              {
                arg_type = Repr.string_of_scheme (generalized, t);
                arg_default = Option.map Value.to_string d;
                arg_description = doc;
              } ))
        proto
    in
    let methods =
      List.map
        (fun (m : Type.meth) ->
          let d = m.doc in
          let d = if d = "" then None else Some d in
          ( m.meth,
            Doc.Value.
              {
                meth_type = Repr.string_of_scheme m.scheme;
                meth_description = d;
              } ))
        meth
    in
    Doc.Value.
      {
        typ = Repr.string_of_scheme (generalized, t);
        category;
        flags;
        description = descr;
        examples;
        arguments;
        methods;
      }
    (* to_plugin_doc category flags examples descr proto return_t *)
  in
  let doc = Lazy.from_fun doc in
  let generalized = Typing.filter_vars (fun _ -> true) t in
  Environment.add_builtin ~doc
    (String.split_on_char '.' name)
    ((generalized, t), value);
  name

let add_builtin_base ~category ~descr ?(flags = []) ?base name value t =
  let name = mk_module_name ?base name in
  let value = { pos = t.Type.pos; value } in
  let generalized = Typing.filter_vars (fun _ -> true) t in
  let doc () =
    Doc.Value.
      {
        typ = Repr.string_of_scheme (generalized, t);
        category;
        flags;
        description = descr;
        examples = [];
        arguments = [];
        methods = [];
      }
  in
  Environment.add_builtin ~doc:(Lazy.from_fun doc)
    (String.split_on_char '.' name)
    ((generalized, t), value);
  name

let add_module ?base name =
  let name = mk_module_name ?base name in
  Environment.add_module (String.split_on_char '.' name);
  name

let module_name name = name

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

let to_ref t = match (demeth t).value with Ref r -> r | _ -> assert false
let to_string_list l = List.map to_string (to_list l)
let to_int_list l = List.map to_int (to_list l)

let to_getter t =
  match (demeth t).value with
    | Fun ([], _, _) | FFI ([], _) -> fun () -> apply t []
    | _ -> fun () -> t

let is_function t =
  match (demeth t).value with
    | Fun ([], _, _) | FFI ([], _) -> true
    | _ -> false

(** [assoc lbl n l] returns the [n]th element in [l]
  * of which the first component is [lbl]. *)
let rec assoc label n = function
  | [] -> raise Not_found
  | (l, e) :: tl ->
      if l = label then if n = 1 then e else assoc label (n - 1) tl
      else assoc label n tl

let raise_error = Runtime_error.raise

let runtime_error_of_exception ~bt ~kind exn =
  match exn with
    | Runtime_error.Runtime_error error -> error
    | _ ->
        Runtime_error.make ~pos:[]
          ~message:
            (Printf.sprintf "%s\nBacktrace:\n%s" (Printexc.to_string exn)
               (Printexc.raw_backtrace_to_string bt))
          kind

let raise_as_runtime ~bt ~kind exn =
  match exn with
    | Runtime_error.Runtime_error _ -> Printexc.raise_with_backtrace exn bt
    | _ ->
        Printexc.raise_with_backtrace
          (Runtime_error.Runtime_error
             (runtime_error_of_exception ~bt ~kind exn))
          bt

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

(* This is used to pass position in application environment. *)
module Single_position = struct
  let t =
    method_t unit_t
      [
        ("filename", ([], string_t), "filename");
        ("line_number", ([], int_t), "line number");
        ("character_offset", ([], int_t), "character offset");
      ]

  let to_value { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } =
    meth unit
      [
        ("filename", string pos_fname);
        ("line_number", int pos_lnum);
        ("character_offset", int (pos_cnum - pos_bol));
      ]

  let of_value v =
    {
      Lexing.pos_fname = to_string (invoke v "filename");
      pos_lnum = to_int (invoke v "line_number");
      pos_bol = 0;
      pos_cnum = to_int (invoke v "character_offset");
    }
end

module Position = struct
  let t =
    method_t unit_t
      [
        ("position_start", ([], Single_position.t), "Starting position");
        ("position_end", ([], Single_position.t), "Ending position");
        ( "to_string",
          ([], fun_t [(true, "prefix", string_t)] string_t),
          "Render as string" );
      ]

  let to_value (start, _end) =
    meth unit
      [
        ("position_start", Single_position.to_value start);
        ("position_end", Single_position.to_value _end);
        ( "to_string",
          val_fun
            [("prefix", "prefix", Some (string "At "))]
            (fun p ->
              let prefix = to_string (List.assoc "prefix" p) in
              string (Pos.to_string ~prefix (start, _end))) );
      ]

  let of_value v =
    ( Single_position.of_value (invoke v "position_start"),
      Single_position.of_value (invoke v "position_end") )
end

module Stacktrace = struct
  let t = list_t Position.t
  let to_value l = list (List.map Position.to_value l)
  let of_value v = List.map Position.of_value (to_list v)
end

let pos_var = "_pos_"
let pos env = Stacktrace.of_value (List.assoc pos_var env)
