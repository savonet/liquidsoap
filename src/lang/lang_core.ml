(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
module Custom = Term.Custom
module Methods = Term.Methods

type t = Type.t
type module_name = string
type scheme = Type.scheme
type value = Value.t

(** Type construction *)

let int_t = Type.make Type.Int
let unit_t = Type.make Type.unit
let float_t = Type.make Type.Float
let bool_t = Type.make Type.Bool
let string_t = Type.make Type.String
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
let univ_t ?(constraints = []) () = Type.var ~constraints ()
let getter_t a = Type.make (Type.Getter a)
let ref_t a = Type.reference a

(** Value construction *)

let mk = Value.make
let unit = mk Value.unit
let int i = mk (`Int i)
let octal_int i = mk ~flags:Flags.(add empty octal_int) (`Int i)
let hex_int i = mk ~flags:Flags.(add empty hex_int) (`Int i)
let bool i = mk (`Bool i)
let float i = mk (`Float i)
let string i = mk (`String i)
let tuple l = mk (`Tuple l)
let product a b = tuple [a; b]
let list l = mk (`List l)
let null = mk `Null

let meth v l =
  Value.map_methods v (fun methods ->
      List.fold_left (fun v (k, m) -> Methods.add k m v) methods l)

let record = meth (mk (`Tuple []))
let val_fun p f = mk (`FFI { ffi_args = p; ffi_fn = f })
let term_fun p tm = mk (`Fun { fun_args = p; fun_env = []; fun_body = tm })

let val_cst_fun p c =
  let p = List.map (fun (l, d) -> (l, "_", d)) p in
  let f t tm =
    let tm = Term.make ~t tm in
    mk (`Fun { fun_args = p; fun_env = []; fun_body = tm })
  in
  let mkg g = Type.make g in
  (* Convert the value into a term if possible, to enable introspection, mostly
     for printing. *)
    match c with
    | Null _ -> f (Type.var ()) `Null
    | Tuple { value = [] } -> f (Type.make Type.unit) Term.unit
    | Int { value = i } -> f (mkg Type.Int) (`Int i)
    | Bool { value = i } -> f (mkg Type.Bool) (`Bool i)
    | Float { value = i } -> f (mkg Type.Float) (`Float i)
    | String { value = i } -> f (mkg Type.String) (`String i)
    | _ -> mk (`FFI { ffi_args = p; ffi_fn = (fun _ -> c) })

let reference get set =
  let get = val_fun [] (fun _ -> get ()) in
  let set =
    val_fun
      [("", "", None)]
      (fun p ->
        List.assoc "" p |> set;
        unit)
  in
  meth get [("set", set)]

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

type 'a meth = { name : string; scheme : scheme; descr : string; value : 'a }

let add_builtin ~category ~descr ?(flags = []) ?(meth = []) ?(callbacks = [])
    ?(examples = []) ?base name proto return_t f =
  let name = mk_module_name ?base name in
  let return_t =
    let meth =
      List.map (fun { name; scheme; descr } -> (name, scheme, descr)) meth
    in
    method_t return_t meth
  in
  let f =
    if meth = [] then f
    else (
      let meth = List.map (fun { name; value } -> (name, value)) meth in
      fun p -> meth_fun (f p) meth)
  in
  let t = builtin_type proto return_t in
  let value =
    mk
      (`FFI
         {
           ffi_args = List.map (fun (lbl, _, opt, _) -> (lbl, lbl, opt)) proto;
           ffi_fn = f;
         })
  in
  let doc () =
    let meth, return_t = Type.split_meths return_t in
    let callbacks, meth =
      List.partition
        (fun (m : Type.meth) ->
          if List.mem m.meth callbacks then m.doc.category <- `Callback;
          m.doc.category = `Callback)
        meth
    in
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
          let d = m.doc.meth_descr in
          let d = if d = "" then None else Some d in
          ( m.meth,
            Doc.Value.
              {
                meth_type = Repr.string_of_scheme m.scheme;
                meth_description = d;
              } ))
        meth
    in
    let callbacks =
      List.map
        (fun (m : Type.meth) ->
          let d = m.doc.meth_descr in
          let d = if d = "" then None else Some d in
          ( m.meth,
            Doc.Value.
              {
                meth_type = Repr.string_of_scheme m.scheme;
                meth_description = d;
              } ))
        callbacks
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
        callbacks;
      }
    (* to_plugin_doc category flags examples descr proto return_t *)
  in
  let doc = Lazy.from_fun doc in
  let generalized = Typing.filter_vars (fun _ -> true) t in
  Environment.add_builtin ~doc
    (String.split_on_char '.' name)
    ((generalized, t), value);
  name

let add_builtin_value ~category ~descr ?(flags = []) ?base name value t =
  let name = mk_module_name ?base name in
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
        callbacks = [];
      }
  in
  Environment.add_builtin ~doc:(Lazy.from_fun doc)
    (String.split_on_char '.' name)
    ((generalized, t), value);
  name

let add_builtin_base ~category ~descr ?flags ?base name value t =
  add_builtin_value ~category ~descr ?flags ?base name (make value) t

let add_module ?base name =
  let name = mk_module_name ?base name in
  Environment.add_module (String.split_on_char '.' name);
  name

let module_name name = name

(* Delay this function in order not to have Lang depend on Evaluation. *)
let apply_fun : (?pos:Pos.t list -> value -> env -> value) ref =
  ref (fun ?pos:_ _ -> assert false)

let apply ?pos f p = !apply_fun ?pos f p [@@inline always]

(** {1 High-level manipulation of values} *)

let to_unit = function Tuple { value = [] } -> () | _ -> assert false
[@@inline always]

let to_bool = function Bool { value = b } -> b | _ -> assert false
[@@inline always]

let to_bool_getter = function
  | Bool { value = b } -> fun () -> b
  | (Fun _ as v) | (FFI _ as v) -> (
      fun () ->
        match apply v [] with Bool { value = b } -> b | _ -> assert false)
  | _ -> assert false
[@@inline always]

let to_fun v = apply v [@@inline always]

let to_string = function String { value = s } -> s | _ -> assert false
[@@inline always]

let to_string_getter = function
  | String { value = s } -> fun () -> s
  | (Fun _ as v) | (FFI _ as v) -> (
      fun () ->
        match apply v [] with String { value = s } -> s | _ -> assert false)
  | _ -> assert false
[@@inline always]

let to_float = function Float { value = f } -> f | _ -> assert false
[@@inline always]

let to_float_getter = function
  | Float { value = f } -> fun () -> f
  | (Fun _ as v) | (FFI _ as v) -> (
      fun () ->
        match apply v [] with Float { value = f } -> f | _ -> assert false)
  | _ -> assert false
[@@inline always]

let to_int = function Int { value = i } -> i | _ -> assert false
[@@inline always]

let to_int_getter = function
  | Int { value = i } -> fun () -> i
  | (Fun _ as v) | (FFI _ as v) -> (
      fun () ->
        match apply v [] with Int { value = i } -> i | _ -> assert false)
  | _ -> assert false
[@@inline always]

let to_num = function
  | Int { value = i } -> `Int i
  | Float { value = f } -> `Float f
  | _ -> assert false
[@@inline always]

let to_list = function List { value = l } -> l | _ -> assert false
[@@inline always]

let to_tuple = function Tuple { value = l } -> l | _ -> assert false
[@@inline always]

let to_option = function Null _ -> None | v -> Some v [@@inline always]

let to_valued_option convert v = Option.map convert (to_option v)
[@@inline always]

let to_default_option ~default convert v =
  Option.value ~default (to_valued_option convert v)
[@@inline always]

let to_product = function
  | Tuple { value = [a; b] } -> (a, b)
  | _ -> assert false
[@@inline always]

let to_string_list l = List.map to_string (to_list l) [@@inline always]
let to_int_list l = List.map to_int (to_list l) [@@inline always]

let to_getter = function
  | (Fun { fun_args = [] } as v) | (FFI { ffi_args = []; _ } as v) ->
      fun () -> apply v []
  | v -> fun () -> v
[@@inline always]

let to_ref t =
  let m, t = split_meths t in
  let get = to_getter t in
  let set =
    let f = List.assoc "set" m in
    fun x -> ignore (apply f [("", x)])
  in
  (get, set)

let to_valued_ref getc setc t =
  let get, set = to_ref t in
  ((fun () -> getc (get ())), fun x -> set (setc x))

(** [assoc lbl n l] returns the [n]th element in [l] of which the first
    component is [lbl]. *)
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
        let pos =
          match Printexc.backtrace_slots bt with
            | None -> []
            | Some entries ->
                List.fold_left
                  (fun pos slot ->
                    match Printexc.Slot.location slot with
                      | None -> pos
                      | Some
                          {
                            Printexc.filename = pos_fname;
                            line_number = pos_lnum;
                            start_char = pos_bol;
                            end_char = pos_cnum;
                          } ->
                          let p =
                            { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum }
                          in
                          Pos.of_lexing_pos (p, p) :: pos)
                  []
                  (List.rev (Array.to_list entries))
        in
        Runtime_error.make ~pos ~message:(Printexc.to_string exn) kind

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
     no '=' could be found. *)
  let split s =
    try
      let pos = String.index s '=' in
      (String.sub s 0 pos, String.sub s (pos + 1) (String.length s - pos - 1))
    with _ -> (s, "")
  in
  let l = Array.to_list l in
  List.map split l

module Position = struct
  let t =
    method_t unit_t
      [
        ("filename", ([], string_t), "Filename");
        ("lstart", ([], int_t), "Starting line");
        ("lstop", ([], int_t), "Stopping line");
        ("cstart", ([], int_t), "Starting character");
        ("cstop", ([], int_t), "Stopping character");
        ( "to_string",
          ([], fun_t [(true, "prefix", string_t)] string_t),
          "Render as string" );
      ]

  let to_value pos =
    let { Pos.fname; lstart; lstop; cstart; cstop } = Pos.unpack pos in
    meth unit
      [
        ("filename", string fname);
        ("lstart", int lstart);
        ("lstop", int lstop);
        ("cstart", int cstart);
        ("cstop", int cstop);
        ( "to_string",
          val_fun
            [("prefix", "prefix", Some (string "At "))]
            (fun p ->
              let prefix = to_string (List.assoc "prefix" p) in
              string (Pos.to_string ~prefix pos)) );
      ]

  let of_value v =
    let fname = to_string (invoke v "filename") in
    let lstart = to_int (invoke v "lstart") in
    let lstop = to_int (invoke v "lstop") in
    let cstart = to_int (invoke v "cstart") in
    let cstop = to_int (invoke v "cstop") in
    Pos.pack { fname; lstart; lstop; cstart; cstop }
end

module Stacktrace = struct
  let t = list_t Position.t
  let to_value l = list (List.map Position.to_value l)
  let of_value v = List.map Position.of_value (to_list v)
end

let pos_var = "_pos_"
let pos env = Stacktrace.of_value (List.assoc pos_var env)
