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

(** Terms and values in the Liquidsoap language. *)

(** An internal error. Those should not happen in theory... *)
exception Internal_error of (Pos.t list * string)

(** A parsing error. *)
exception Parse_error of (Pos.t * string)

(** Unsupported format *)
exception Unsupported_format of (Pos.t option * string)

let () =
  Printexc.register_printer (function
    | Internal_error (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Internal_error at %s: %s"
             (Pos.List.to_string pos) e)
    | Parse_error (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Parse_error at %s: %s"
             (Pos.to_string pos) e)
    | Unsupported_format (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Unsupported_format at %s: %s"
             (Pos.Option.to_string pos) e)
    | _ -> None)

let conf_debug = ref false
let conf_debug_errors = ref false

(** Are we in debugging mode? *)
let debug =
  Lazy.from_fun (fun () ->
      try
        ignore (Sys.getenv "LIQUIDSOAP_DEBUG_LANG");
        true
      with Not_found -> !conf_debug)

(* We want to keep this a reference and not a dtools and not something more
   complicated (e.g. dtools) in order not to impact performances. *)
let profile = ref false

let ref_t ?pos t =
  Type.make ?pos
    (* The type has to be invariant because we don't want the sup mechanism to be used here, see #2806. *)
    (Type.Constr { Type.constructor = "ref"; params = [(`Invariant, t)] })

(** {2 Terms} *)

(** Sets of variables. *)
module Vars = Set.Make (String)

module Ground = struct
  type t = ..

  type content = {
    descr : t -> string;
    to_json : pos:Pos.t list -> t -> Json.t;
    compare : t -> t -> int;
    typ : (module Type.Ground.Custom);
  }

  let handlers = Hashtbl.create 10

  let register matcher c =
    let module C = (val c.typ : Type.Ground.Custom) in
    Hashtbl.replace handlers C.Type (c, matcher)

  exception Found of content

  let find v =
    try
      Hashtbl.iter
        (fun _ (c, matcher) -> if matcher v then raise (Found c))
        handlers;
      raise Not_found
    with Found c -> c

  let to_string (v : t) = (find v).descr v
  let to_json (v : t) = (find v).to_json v

  let to_descr (v : t) =
    let module C = (val (find v).typ : Type.Ground.Custom) in
    C.descr

  let to_type (v : t) =
    let module C = (val (find v).typ : Type.Ground.Custom) in
    C.Type

  let compare (v : t) = (find v).compare v

  type t += Bool of bool | Int of int | String of string | Float of float

  let () =
    let compare conv v v' = Stdlib.compare (conv v) (conv v') in
    let to_bool = function Bool b -> b | _ -> assert false in
    let to_string b = string_of_bool (to_bool b) in
    let to_json ~pos:_ b = `Bool (to_bool b) in
    register
      (function Bool _ -> true | _ -> false)
      {
        descr = to_string;
        to_json;
        compare = compare to_bool;
        typ = (module Type.Ground.Bool : Type.Ground.Custom);
      };
    let to_int = function Int i -> i | _ -> assert false in
    let to_string i = string_of_int (to_int i) in
    let to_json ~pos:_ i = `Int (to_int i) in
    register
      (function Int _ -> true | _ -> false)
      {
        descr = to_string;
        to_json;
        compare = compare to_int;
        typ = (module Type.Ground.Int : Type.Ground.Custom);
      };
    let to_string = function
      | String s -> Lang_string.quote_string s
      | _ -> assert false
    in
    let to_json ~pos:_ = function String s -> `String s | _ -> assert false in
    register
      (function String _ -> true | _ -> false)
      {
        descr = to_string;
        to_json;
        compare = compare (function String s -> s | _ -> assert false);
        typ = (module Type.Ground.String : Type.Ground.Custom);
      };
    let to_float = function Float f -> f | _ -> assert false in
    let to_json ~pos:_ f = `Float (to_float f) in
    register
      (function Float _ -> true | _ -> false)
      {
        descr = (fun f -> string_of_float (to_float f));
        to_json;
        compare = compare to_float;
        typ = (module Type.Ground.Float : Type.Ground.Custom);
      }
end

module type GroundDef = sig
  type content

  val descr : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
  val typ : (module Type.Ground.Custom)
end

module MkGround (D : GroundDef) = struct
  type Ground.t += Ground of D.content

  let () =
    let to_ground = function Ground g -> g | _ -> assert false in
    let to_json ~pos v = D.to_json ~pos (to_ground v) in
    let compare v v' = D.compare (to_ground v) (to_ground v') in
    let descr v = D.descr (to_ground v) in
    Ground.register
      (function Ground _ -> true | _ -> false)
      { Ground.typ = D.typ; to_json; compare; descr }
end

type t = { mutable t : Type.t; term : in_term }

(** Documentation for declarations: general documentation, parameters, methods. *)
and doc = Doc.Value.t

and let_t = {
  doc : doc option;
  (* name, arguments, methods *)
  replace : bool;
  (* whether the definition replaces a previously existing one (keeping methods) *)
  pat : pattern;
  mutable gen : Type.var list;
  def : t;
  body : t;
}

and encoder_params = (string * [ `Term of t | `Encoder of encoder ]) list

(** A formal encoder. *)
and encoder = string * encoder_params

and invoke = { invoked : t; default : t option; meth : string }
and meth = { name : string; meth_value : t }

and in_term =
  | Ground of Ground.t
  | Encoder of encoder
  | List of t list
  | Tuple of t list
  | Any
  | Null
  | Cast of t * Type.t
  | Meth of meth * t
  | Invoke of invoke
  | Open of t * t
  | Let of let_t
  | Var of string
  | Seq of t * t
  | App of t * (string * t) list
  (* [fun ~l1:x1 .. ?li:(xi=defi) .. -> body] =
   * [Fun (V, [(l1,x1,None)..(li,xi,Some defi)..], body)]
   * The first component [V] is the list containing all
   * variables occurring in the function. It is used to
   * restrict the environment captured when a closure is
   * formed. *)
  | Fun of Vars.t * (string * string * Type.t * t option) list * t
  (* A recursive function, the first string is the name of the recursive
     variable. *)
  | RFun of string * Vars.t * (string * string * Type.t * t option) list * t

and pattern =
  | PVar of string list  (** a field *)
  | PTuple of pattern list  (** a tuple *)
  | PList of (pattern list * string option * pattern list)  (** a list *)
  | PMeth of (pattern option * (string * pattern option) list)
      (** a value with methods *)

type term = t

let unit = Tuple []

(* Only used for printing very simple functions. *)
let is_ground x = match x.term with Ground _ -> true | _ -> false

let rec string_of_pat = function
  | PVar l -> String.concat "." l
  | PTuple l -> "(" ^ String.concat ", " (List.map string_of_pat l) ^ ")"
  | PList (l, spread, l') ->
      "["
      ^ String.concat ", "
          (List.map string_of_pat l
          @ (match spread with None -> [] | Some v -> ["..." ^ v])
          @ List.map string_of_pat l')
      ^ "]"
  | PMeth (pat, l) ->
      (match pat with None -> "" | Some pat -> string_of_pat pat ^ ".")
      ^ "{"
      ^ String.concat ", "
          (List.map
             (fun (lbl, pat) ->
               match pat with
                 | None -> lbl
                 | Some pat -> lbl ^ ": " ^ string_of_pat pat)
             l)
      ^ "}"

(** String representation of terms, (almost) assuming they are in normal
    form. *)
let rec to_string v =
  match v.term with
    | Ground g -> Ground.to_string g
    | Encoder e ->
        let rec aux (e, p) =
          let p =
            p
            |> List.map (function
                 | "", `Term v -> to_string v
                 | l, `Term v -> l ^ "=" ^ to_string v
                 | _, `Encoder e -> aux e)
            |> String.concat ", "
          in
          "%" ^ e ^ "(" ^ p ^ ")"
        in
        aux e
    | List l -> "[" ^ String.concat ", " (List.map to_string l) ^ "]"
    | Tuple l -> "(" ^ String.concat ", " (List.map to_string l) ^ ")"
    | Any -> "any"
    | Null -> "null"
    | Cast (e, t) -> "(" ^ to_string e ^ " : " ^ Repr.string_of_type t ^ ")"
    | Meth ({ name = l; meth_value = v }, e) ->
        to_string e ^ ".{" ^ l ^ " = " ^ to_string v ^ "}"
    | Invoke { invoked = e; meth = l; default } -> (
        match default with
          | None -> to_string e ^ "." ^ l
          | Some v -> "(" ^ to_string e ^ "." ^ l ^ " ?? " ^ to_string v ^ ")")
    | Open (m, e) -> "open " ^ to_string m ^ " " ^ to_string e
    | Fun (_, [], v) when is_ground v -> "{" ^ to_string v ^ "}"
    | Fun _ | RFun _ -> "<fun>"
    | Var s -> s
    | App (hd, tl) ->
        let tl =
          List.map
            (fun (lbl, v) ->
              (if lbl = "" then "" else lbl ^ " = ") ^ to_string v)
            tl
        in
        to_string hd ^ "(" ^ String.concat "," tl ^ ")"
    (* | Let _ | Seq _ -> assert false *)
    | Let l ->
        Printf.sprintf "let %s = %s in %s" (string_of_pat l.pat)
          (to_string l.def) (to_string l.body)
    | Seq (e, e') -> to_string e ^ "; " ^ to_string e'

(** Create a new value. *)
let make ?pos ?t e =
  let t = match t with Some t -> t | None -> Type.var ?pos () in
  if Lazy.force debug then
    Printf.eprintf "%s (%s): assigned type var %s\n"
      (Pos.Option.to_string t.Type.pos)
      (try to_string { t; term = e } with _ -> "<?>")
      (Repr.string_of_type t);
  { t; term = e }

let rec free_vars_pat = function
  | PVar [] -> assert false
  | PVar [_] -> Vars.empty
  | PVar (x :: _) -> Vars.singleton x
  | PTuple l -> List.fold_left Vars.union Vars.empty (List.map free_vars_pat l)
  | PList (l, spread, l') ->
      List.fold_left Vars.union Vars.empty
        (List.map free_vars_pat
           (l @ (match spread with None -> [] | Some v -> [PVar [v]]) @ l'))
  | PMeth (pat, l) ->
      List.fold_left Vars.union
        (match pat with None -> Vars.empty | Some pat -> free_vars_pat pat)
        (List.map free_vars_pat
           (List.fold_left
              (fun cur (lbl, pat) ->
                [PVar [lbl]]
                @ (match pat with None -> [] | Some pat -> [pat])
                @ cur)
              [] l))

let rec bound_vars_pat = function
  | PVar [] -> assert false
  | PVar [x] -> Vars.singleton x
  | PVar _ -> Vars.empty
  | PTuple l -> List.fold_left Vars.union Vars.empty (List.map bound_vars_pat l)
  | PList (l, spread, l') ->
      List.fold_left Vars.union Vars.empty
        (List.map bound_vars_pat
           (l @ (match spread with None -> [] | Some v -> [PVar [v]]) @ l'))
  | PMeth (pat, l) ->
      List.fold_left Vars.union
        (match pat with None -> Vars.empty | Some pat -> bound_vars_pat pat)
        (List.map bound_vars_pat
           (List.fold_left
              (fun cur (lbl, pat) ->
                [PVar [lbl]]
                @ (match pat with None -> [] | Some pat -> [pat])
                @ cur)
              [] l))

let rec free_vars tm =
  match tm.term with
    | Ground _ -> Vars.empty
    | Var x -> Vars.singleton x
    | Tuple l ->
        List.fold_left (fun v a -> Vars.union v (free_vars a)) Vars.empty l
    | Any | Null -> Vars.empty
    | Encoder e ->
        let rec enc (_, p) =
          List.fold_left
            (fun v (_, t) ->
              match t with
                | `Term t -> Vars.union v (free_vars t)
                | `Encoder e -> Vars.union v (enc e))
            Vars.empty p
        in
        enc e
    | Cast (e, _) -> free_vars e
    | Seq (a, b) -> Vars.union (free_vars a) (free_vars b)
    | Meth ({ meth_value = v }, e) -> Vars.union (free_vars v) (free_vars e)
    | Invoke { invoked = e } -> free_vars e
    | Open (a, b) -> Vars.union (free_vars a) (free_vars b)
    | List l ->
        List.fold_left (fun v t -> Vars.union v (free_vars t)) Vars.empty l
    | App (hd, l) ->
        List.fold_left
          (fun v (_, t) -> Vars.union v (free_vars t))
          (free_vars hd) l
    | RFun (_, fv, _, _) | Fun (fv, _, _) -> fv
    | Let l ->
        Vars.union (free_vars l.def)
          (Vars.diff (free_vars l.body) (bound_vars_pat l.pat))

let free_vars ?(bound = []) body =
  Vars.diff (free_vars body) (Vars.of_list bound)

(** Values which can be ignored (and will thus not raise a warning if
   ignored). *)
let can_ignore t =
  match (Type.demeth t).Type.descr with
    | Type.Tuple [] | Type.Var _ -> true
    | _ -> false

(** {1 Basic checks and errors} *)

(** Trying to use an unbound variable. *)
exception Unbound of Pos.Option.t * string

(** Silently discarding a meaningful value. *)
exception Ignored of t

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
    passed to the function [f] because the latter has no label [lbl].  The
    [first] information tells whether [lbl=x] is the first parameter with label
    [lbl] in the considered application, which makes the message a bit more
    helpful. *)
exception No_label of t * string * bool * t

(** A function defines multiple arguments with the same label. *)
exception Duplicate_label of Pos.Option.t * string

(** Some mandatory arguments with given label and typed were not passed to the
    function during an application. *)
exception Missing_arguments of Pos.Option.t * (string * Type.t) list

(** Check that all let-bound variables are used. No check is performed for
    variable arguments. This cannot be done at parse-time (as for the
    computation of the free variables of functions) because we need types, as
    well as the ability to distinguish toplevel and inner let-in terms. *)
exception Unused_variable of (string * Pos.t)

let check_unused ~throw ~lib tm =
  let rec check ?(toplevel = false) v tm =
    match tm.term with
      | Var s -> Vars.remove s v
      | Ground _ -> v
      | Tuple l -> List.fold_left (fun a -> check a) v l
      | Any | Null -> v
      | Cast (e, _) -> check v e
      | Meth ({ meth_value = f }, e) -> check (check v e) f
      | Invoke { invoked = e } -> check v e
      | Open (a, b) -> check (check v a) b
      | Seq (a, b) -> check ~toplevel (check v a) b
      | List l -> List.fold_left (fun x y -> check x y) v l
      | Encoder e ->
          let rec enc v (_, p) =
            List.fold_left
              (fun v (_, t) ->
                match t with `Term t -> check v t | `Encoder e -> enc v e)
              v p
          in
          enc v e
      | App (hd, l) ->
          let v = check v hd in
          List.fold_left (fun v (_, t) -> check v t) v l
      | RFun (_, arg, p, body) -> check v { tm with term = Fun (arg, p, body) }
      | Fun (_, p, body) ->
          let v =
            List.fold_left
              (fun v -> function
                | _, _, _, Some default -> check v default
                | _ -> v)
              v p
          in
          let bound =
            List.fold_left (fun v (_, var, _, _) -> Vars.add var v) Vars.empty p
          in
          let masked = Vars.inter v bound in
          let v = Vars.union v bound in
          let v = check v body in
          Vars.iter
            (fun x ->
              if Vars.mem x v && x <> "_" then
                throw (Unused_variable (x, Option.get tm.t.Type.pos)))
            bound;
          (* Restore masked variables. The masking variables have been used but
             it does not count for the ones they masked. Bound variables have
             been handled above. *)
          Vars.union masked (Vars.diff v bound)
      | Let { pat; def; body; _ } ->
          let v = check v def in
          let bvpat = bound_vars_pat pat in
          let mask = Vars.inter v bvpat in
          let v = Vars.union v bvpat in
          let v = check ~toplevel v body in
          if
            (* Do not check for anything at toplevel in libraries *)
            not (toplevel && lib)
          then
            Vars.iter
              (fun s ->
                (* Do we have an unused definition? *)
                if Vars.mem s v then
                  (* There are exceptions: unit and functions when
                     at toplevel (sort of a lib situation...) *)
                  if
                    s <> "_"
                    && not (can_ignore def.t || (toplevel && Type.is_fun def.t))
                  then throw (Unused_variable (s, Option.get tm.t.Type.pos)))
              bvpat;
          Vars.union v mask
  in
  (* Unused free variables may remain *)
  ignore (check ~toplevel:true Vars.empty tm)

(* Abstract types. *)

module type Abstract = sig
  type content

  val t : Type.t
  val to_ground : content -> Ground.t
  val of_ground : Ground.t -> content
  val is_ground : Ground.t -> bool
  val to_term : content -> t
  val of_term : t -> content
  val is_term : t -> bool
end

module type AbstractDef = sig
  type content

  val name : string
  val to_json : pos:Pos.t list -> content -> Json.t
  val descr : content -> string
  val compare : content -> content -> int
end

module MkAbstract (Def : AbstractDef) = struct
  module T = Type.Ground.Make (struct
    let name = Def.name
  end)

  type Ground.t += Value of Def.content

  let () =
    let to_value = function Value v -> v | _ -> assert false in
    let compare v v' = Def.compare (to_value v) (to_value v') in
    let descr v = Def.descr (to_value v) in
    let to_json ~pos v = Def.to_json ~pos (to_value v) in
    Ground.register
      (function Value _ -> true | _ -> false)
      { Ground.descr; to_json; compare; typ = (module T : Type.Ground.Custom) }

  type content = Def.content

  let t = Type.make T.descr
  let of_ground = function Value c -> c | _ -> assert false
  let to_ground c = Value c
  let is_ground = function Value _ -> true | _ -> false
  let of_term t = match t.term with Ground (Value c) -> c | _ -> assert false
  let to_term c = { t = Type.make T.descr; term = Ground (Value c) }
  let is_term t = match t.term with Ground (Value _) -> true | _ -> false
end
