(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** An error at runtime. *)
include Runtime_error

(** An internal error. Those should not happen in theory... *)
exception Internal_error of (pos list * string)

(** A parsing error. *)
exception Parse_error of (pos * string)

(** Unsupported format *)
exception Unsupported_format of (pos option * string)

let () =
  Printexc.register_printer (function
    | Internal_error (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Internal_error at %s: %s"
             (Runtime_error.print_pos_list pos)
             e)
    | Parse_error (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Parse_error at %s: %s"
             (Runtime_error.print_pos pos)
             e)
    | Unsupported_format (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Unsupported_format at %s: %s"
             (Runtime_error.print_pos_opt pos)
             e)
    | _ -> None)

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "lang") "Language configuration."

let conf_debug =
  Dtools.Conf.bool ~p:(conf#plug "debug") ~d:false
    "Debug language features such as type inference and reduction."

let conf_debug_errors =
  Dtools.Conf.bool ~p:(conf#plug "debug_errors") ~d:false
    "Debug errors by showing stacktraces instead of printing messages."

(** Are we in debugging mode? *)
let debug =
  Lazy.from_fun (fun () ->
      try
        ignore (Sys.getenv "LIQUIDSOAP_DEBUG_LANG");
        true
      with Not_found -> conf_debug#get)

(* We want to keep this a reference and not a dtools and not something more
   complicated (e.g. dtools) in order not to impact performances. *)
let profile = ref false

(** {2 Formats} *)

(* In a sense this could be moved to Type, but I like to keep that part free of
   some specificities of liquidsoap, as much as possible. *)

let ref_t ?pos t =
  Type.make ?pos
    (Type.Constr { Type.constructor = "ref"; params = [(Type.Invariant, t)] })

(** A frame kind type is a purely abstract type representing a
    frame kind. *)
let frame_kind_t ?pos audio video midi =
  Type.make ?pos
    (Type.Constr
       {
         Type.constructor = "stream_kind";
         Type.params =
           [
             (Type.Covariant, audio);
             (Type.Covariant, video);
             (Type.Covariant, midi);
           ];
       })

let kind_t ?pos kind =
  let evar ?(constraints = []) () = Type.var ~constraints ?pos () in
  let mk_format f = Type.make ?pos (Type.Ground (Type.Format f)) in
  match kind with
    | `Any -> evar ()
    | `Internal -> evar ~constraints:[Type.InternalMedia] ()
    | `Kind k ->
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content.string_of_kind k;
               Type.params = [(Type.Covariant, evar ())];
             })
    | `Format f ->
        let k = Content.kind f in
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content.string_of_kind k;
               Type.params = [(Type.Covariant, mk_format f)];
             })

let of_frame_kind_t t =
  let t = Type.deref t in
  match t.Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          Type.params = [(_, audio); (_, video); (_, midi)];
        } ->
        { Frame.audio; video; midi }
    | Type.Var ({ contents = Type.Free _ } as var) ->
        let audio = kind_t `Any in
        let video = kind_t `Any in
        let midi = kind_t `Any in
        var := Type.Link (Type.Invariant, frame_kind_t audio video midi);
        { Frame.audio; video; midi }
    | _ -> assert false

(** Type of audio formats that can encode frame of a given kind. *)
let format_t ?pos k =
  Type.make ?pos
    (Type.Constr
       { Type.constructor = "format"; Type.params = [(Type.Covariant, k)] })

(** Type of sources carrying frames of a given kind. *)
let source_t ?pos k =
  Type.make ?pos
    (Type.Constr
       { Type.constructor = "source"; Type.params = [(Type.Invariant, k)] })

(* Filled in later to avoid dependency cycles. *)
let source_methods_t = ref (fun () : Type.t -> assert false)

let of_source_t t =
  match (Type.deref t).Type.descr with
    | Type.Constr { Type.constructor = "source"; Type.params = [(_, t)] } -> t
    | _ -> assert false

let type_of_format ~pos f =
  let kind = Encoder.kind_of_format f in
  let audio = kind_t ~pos kind.Frame.audio in
  let video = kind_t ~pos kind.Frame.video in
  let midi = kind_t ~pos kind.Frame.midi in
  format_t ~pos (frame_kind_t ~pos audio video midi)

(** {2 Terms} *)

(** The way we implement this mini-language is not very efficient. It should not
    matter, since very little computation is done here. It is mostly used for a
    single run on startup to build the sources, and then sometimes for building
    transitions. Terms are small, no recursion is possible. In order to report
    informative errors, including runtime errors (invalid values of a valid type
    given to a FF) we need to keep a complete AST all the way long.  We actually
    don't need the types anymore after the static checking, but I don't want to
    bother with stripping down to another datatype. *)

(** Sets of variables. *)
module Vars = Set.Make (String)

module Ground = struct
  type t = ..

  type content = {
    descr : unit -> string;
    to_json : unit -> Json.t;
    compare : t -> int;
    typ : Type.ground;
  }

  let handlers = Queue.create ()
  let register fn = Queue.add fn handlers

  exception Found of content

  let find v =
    try
      Queue.iter
        (fun h -> match h v with Some c -> raise (Found c) | None -> ())
        handlers;
      assert false
    with Found c -> c

  let to_string (v : t) = (find v).descr ()
  let to_json (v : t) = (find v).to_json ()
  let to_type (v : t) = (find v).typ
  let compare (v : t) = (find v).compare

  type t += Bool of bool | Int of int | String of string | Float of float

  let () =
    register (function
      | Bool b ->
          let compare = function
            | Bool b' -> Stdlib.compare b b'
            | _ -> assert false
          in
          let to_string () = string_of_bool b in
          let to_json () = `Bool b in
          Some { descr = to_string; to_json; compare; typ = Type.Bool }
      | Int i ->
          let compare = function
            | Int i' -> Stdlib.compare i i'
            | _ -> assert false
          in
          let to_string () = string_of_int i in
          let to_json () = `Int i in
          Some { descr = to_string; to_json; compare; typ = Type.Int }
      | String s ->
          let compare = function
            | String s' -> Stdlib.compare s s'
            | _ -> assert false
          in
          let to_json () = `String s in
          Some
            {
              descr = (fun () -> Utils.quote_string s);
              to_json;
              compare;
              typ = Type.String;
            }
      | Float f ->
          let compare = function
            | Float f' -> Stdlib.compare f f'
            | _ -> assert false
          in
          let to_json () = `Float f in
          Some
            {
              descr = (fun () -> string_of_float f);
              to_json;
              compare;
              typ = Type.Float;
            }
      | _ -> None)
end

module type GroundDef = sig
  type content

  val descr : content -> string
  val to_json : content -> Json.t
  val compare : content -> content -> int
  val typ : Type.ground
end

module MkGround (D : GroundDef) = struct
  type Ground.t += Ground of D.content

  let () =
    Ground.register (function
      | Ground v ->
          let descr () = D.descr v in
          let to_json () = D.to_json v in
          let compare = function
            | Ground v' -> D.compare v v'
            | _ -> assert false
          in
          Some { Ground.typ = D.typ; to_json; compare; descr }
      | _ -> None)
end

type t = { mutable t : Type.t; term : in_term }

and let_t = {
  doc : Doc.item * (string * string) list * (string * string) list;
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

and in_term =
  | Ground of Ground.t
  | Encoder of encoder
  | List of t list
  | Tuple of t list
  | Null
  | Cast of t * Type.t
  | Meth of string * t * t
  | Invoke of t * string
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
  | RFun of string * Vars.t * (string * string * Type.t * t option) list * t

(* A recursive function, the first string is the name of the recursive
   variable. *)
and pattern =
  | PVar of string list  (** a field *)
  | PTuple of pattern list  (** a tuple *)
  | PList of (pattern list * string option * pattern list) (* a list *)
  | PMeth of (pattern option * (string * pattern option) list)
(* a value with methods *)

type term = t

let unit = Tuple []

(* Only used for printing very simple functions. *)
let is_ground x =
  match x.term with Ground _ -> true (* | Ref x -> is_ground x *) | _ -> false

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
    | Null -> "null"
    | Cast (e, t) -> "(" ^ to_string e ^ " : " ^ Repr.string_of_type t ^ ")"
    | Meth (l, v, e) -> to_string e ^ ".{" ^ l ^ " = " ^ to_string v ^ "}"
    | Invoke (e, l) -> to_string e ^ "." ^ l
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
      (Repr.string_of_pos_opt t.Type.pos)
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
    | Null -> Vars.empty
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
    | Meth (_, v, e) -> Vars.union (free_vars v) (free_vars e)
    | Invoke (e, _) -> free_vars e
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

exception Unbound of Type.pos option * string
exception Ignored of t

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
  * passed to the function [f] because the latter has no label [lbl].
  * The [first] information tells whether [lbl=x] is the first parameter with
  * label [lbl] in the considered application, which makes the message a bit
  * more helpful. *)
exception No_label of t * string * bool * t

(** Check that all let-bound variables are used.
  * No check is performed for variable arguments.
  * This cannot be done at parse-time (as for the computatin of the
  * free variables of functions) because we need types, as well as
  * the ability to distinguish toplevel and inner let-in terms. *)
exception Unused_variable of (string * Type.pos)

let check_unused ~throw ~lib tm =
  let rec check ?(toplevel = false) v tm =
    match tm.term with
      | Var s -> Vars.remove s v
      | Ground _ -> v
      | Tuple l -> List.fold_left (fun a -> check a) v l
      | Null -> v
      | Cast (e, _) -> check v e
      | Meth (_, f, e) -> check (check v e) f
      | Invoke (e, _) -> check v e
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
  val to_json : content -> Json.t
  val descr : content -> string
  val compare : content -> content -> int
end

module MkAbstract (Def : AbstractDef) = struct
  type Type.ground += Type
  type Ground.t += Value of Def.content

  let () =
    Ground.register (function
      | Value v ->
          let compare = function
            | Value v' -> Def.compare v v'
            | _ -> assert false
          in
          Some
            {
              Ground.descr = (fun () -> Def.descr v);
              to_json = (fun () -> Def.to_json v);
              compare;
              typ = Type;
            }
      | _ -> None);

    Type.register_ground_printer (function Type -> Some Def.name | _ -> None);
    Type.register_ground_resolver (fun s ->
        if s = Def.name then Some Type else None)

  type content = Def.content

  let t = Type.make (Type.Ground Type)
  let of_ground = function Value c -> c | _ -> assert false
  let to_ground c = Value c
  let is_ground = function Value _ -> true | _ -> false
  let of_term t = match t.term with Ground (Value c) -> c | _ -> assert false
  let to_term c = { t = Type.make (Type.Ground Type); term = Ground (Value c) }
  let is_term t = match t.term with Ground (Value _) -> true | _ -> false
end
