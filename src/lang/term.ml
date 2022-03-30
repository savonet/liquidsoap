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

(** Terms and values in the Liquidsoap language. *)

(** An error at runtime. *)
include Runtime_error

(** An internal error. Those should not happen in theory... *)
exception Internal_error of (pos list * string)

(** A parsing error. *)
exception Parse_error of (pos * string)

(** Unsupported format *)
exception Unsupported_format of (pos * Encoder.format)

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
    | Unsupported_format (pos, f) ->
        Some
          (Printf.sprintf "Lang_values.Unsupported_format at %s: %s"
             (Runtime_error.print_pos pos)
             (Encoder.string_of_format f))
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

let ref_t ?pos ?level t =
  Type.make ?pos ?level
    (Type.Constr { Type.name = "ref"; params = [(Type.Invariant, t)] })

(** A frame kind type is a purely abstract type representing a
    frame kind. *)
let frame_kind_t ?pos ?level audio video midi =
  Type.make ?pos ?level
    (Type.Constr
       {
         Type.name = "stream_kind";
         Type.params =
           [
             (Type.Covariant, audio);
             (Type.Covariant, video);
             (Type.Covariant, midi);
           ];
       })

let kind_t ?pos ?level kind =
  let evar ?(constraints = []) () =
    Type.fresh ~constraints
      ~pos:(match pos with None -> None | Some pos -> pos)
      ~level:(-1)
  in
  let mk_format f = Type.make ?pos ?level (Type.Ground (Type.Format f)) in
  match kind with
    | `Any -> evar ()
    | `Internal -> evar ~constraints:[Type.InternalMedia] ()
    | `Kind k ->
        Type.make ?pos ?level
          (Type.Constr
             {
               Type.name = Frame_content.string_of_kind k;
               Type.params = [(Type.Covariant, evar ())];
             })
    | `Format f ->
        let k = Frame_content.kind f in
        Type.make ?pos ?level
          (Type.Constr
             {
               Type.name = Frame_content.string_of_kind k;
               Type.params = [(Type.Covariant, mk_format f)];
             })

let of_frame_kind_t t =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.name = "stream_kind";
          Type.params = [(_, audio); (_, video); (_, midi)];
        } ->
        { Frame.audio; video; midi }
    | Type.EVar (_, _) ->
        let audio = kind_t `Any in
        let video = kind_t `Any in
        let midi = kind_t `Any in
        Type.bind t (frame_kind_t audio video midi);
        { Frame.audio; video; midi }
    | _ -> assert false

(** Type of audio formats that can encode frame of a given kind. *)
let format_t ?pos ?level k =
  Type.make ?pos ?level
    (Type.Constr { Type.name = "format"; Type.params = [(Type.Covariant, k)] })

(** Type of sources carrying frames of a given kind. *)
let source_t ?pos ?level k =
  Type.make ?pos ?level
    (Type.Constr { Type.name = "source"; Type.params = [(Type.Invariant, k)] })

(* Filled in later to avoid dependency cycles. *)
let source_methods_t = ref (fun () : Type.t -> assert false)

let of_source_t t =
  match (Type.deref t).Type.descr with
    | Type.Constr { Type.name = "source"; Type.params = [(_, t)] } -> t
    | _ -> assert false

let request_t ?pos ?level () = Type.make ?pos ?level (Type.Ground Type.Request)

let type_of_format ~pos ~level f =
  let kind = Encoder.kind_of_format f in
  let audio = kind_t ~pos ~level kind.Frame.audio in
  let video = kind_t ~pos ~level kind.Frame.video in
  let midi = kind_t ~pos ~level kind.Frame.midi in
  format_t ~pos ~level (frame_kind_t ~pos ~level audio video midi)

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
    descr : t -> string;
    to_json : compact:bool -> json5:bool -> t -> string;
    compare : t -> t -> int;
    typ : Type.ground;
  }

  let handlers = Hashtbl.create 10
  let register matcher c = Hashtbl.replace handlers c.typ (c, matcher)

  exception Found of content

  let find v =
    try
      Hashtbl.iter
        (fun _ (c, matcher) -> if matcher v then raise (Found c))
        handlers;
      raise Not_found
    with Found c -> c

  let to_string (v : t) = (find v).descr v
  let to_json ~compact ~json5 (v : t) = (find v).to_json ~compact ~json5 v
  let to_type (v : t) = (find v).typ
  let compare (v : t) = (find v).compare v

  type t +=
    | Bool of bool
    | Int of int
    | String of string
    | Float of float
    | Request of Request.t

  let () =
    let compare conv v v' = Stdlib.compare (conv v) (conv v') in
    let to_bool = function Bool b -> b | _ -> assert false in
    let to_string b = string_of_bool (to_bool b) in
    let to_json ~compact:_ ~json5:_ = to_string in
    register
      (function Bool _ -> true | _ -> false)
      { descr = to_string; to_json; compare = compare to_bool; typ = Type.Bool };
    let to_int = function Int i -> i | _ -> assert false in
    let to_string i = string_of_int (to_int i) in
    let to_json ~compact:_ ~json5:_ = to_string in
    register
      (function Int _ -> true | _ -> false)
      { descr = to_string; to_json; compare = compare to_int; typ = Type.Int };
    let to_string = function
      | String s -> Utils.quote_string s
      | _ -> assert false
    in
    let to_json ~compact:_ ~json5:_ = to_string in
    register
      (function String _ -> true | _ -> false)
      {
        descr = to_string;
        to_json;
        compare = compare (function String s -> s | _ -> assert false);
        typ = Type.String;
      };
    let to_float = function Float f -> f | _ -> assert false in
    let to_json ~compact:_ ~json5 f =
      let f = to_float f in
      match classify_float f with
        | FP_infinite when json5 -> if f < 0. then "-Infinity" else "Infinity"
        | FP_nan when json5 -> if f < 0. then "-NaN" else "NaN"
        | FP_infinite | FP_nan -> "null"
        | _ ->
            let s = string_of_float f in
            let s = Printf.sprintf "%s" s in
            if s.[String.length s - 1] = '.' then Printf.sprintf "%s0" s else s
    in
    register
      (function Float _ -> true | _ -> false)
      {
        descr = (fun f -> string_of_float (to_float f));
        to_json;
        compare = compare to_float;
        typ = Type.Float;
      };
    let to_request = function Request r -> r | _ -> assert false in
    let descr r =
      Printf.sprintf "<request(id=%d)>" (Request.get_id (to_request r))
    in
    let to_json ~compact:_ ~json5:_ r = Printf.sprintf "%S" (descr r) in
    let compare r r' =
      Stdlib.compare
        (Request.get_id (to_request r))
        (Request.get_id (to_request r'))
    in
    register
      (function Request _ -> true | _ -> false)
      { descr; compare; to_json; typ = Type.Request }
end

module type GroundDef = sig
  type content

  val descr : content -> string
  val to_json : compact:bool -> json5:bool -> content -> string
  val compare : content -> content -> int
  val typ : Type.ground
end

module MkGround (D : GroundDef) = struct
  type Ground.t += Ground of D.content

  let () =
    let to_ground = function Ground g -> g | _ -> assert false in
    let to_json ~compact ~json5 v = D.to_json ~compact ~json5 (to_ground v) in
    let compare v v' = D.compare (to_ground v) (to_ground v') in
    let descr v = D.descr (to_ground v) in
    Ground.register
      (function Ground _ -> true | _ -> false)
      { Ground.typ = D.typ; to_json; compare; descr }
end

type t = { mutable t : Type.t; term : in_term }

and let_t = {
  doc : Doc.item * (string * string) list * (string * string) list;
  (* name, arguments, methods *)
  replace : bool;
  (* whether the definition replaces a previously existing one (keeping methods) *)
  pat : pattern;
  mutable gen : (int * Type.constraints) list;
  def : t;
  body : t;
}

and in_term =
  | Ground of Ground.t
  | Encoder of Encoder.format
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

type term = t

let unit = Tuple []

(* Only used for printing very simple functions. *)
let is_ground x =
  match x.term with
    | Ground _ | Encoder _ -> true
    (* | Ref x -> is_ground x *)
    | _ -> false

(** Print terms, (almost) assuming they are in normal form. *)
let rec print_term v =
  match v.term with
    | Ground g -> Ground.to_string g
    | Encoder e -> Encoder.string_of_format e
    | List l -> "[" ^ String.concat ", " (List.map print_term l) ^ "]"
    | Tuple l -> "(" ^ String.concat ", " (List.map print_term l) ^ ")"
    | Null -> "null"
    | Cast (e, t) -> "(" ^ print_term e ^ " : " ^ Type.print t ^ ")"
    | Meth (l, v, e) -> print_term e ^ ".{" ^ l ^ " = " ^ print_term v ^ "}"
    | Invoke (e, l) -> print_term e ^ "." ^ l
    | Open (m, e) -> "open " ^ print_term m ^ " " ^ print_term e
    | Fun (_, [], v) when is_ground v -> "{" ^ print_term v ^ "}"
    | Fun _ | RFun _ -> "<fun>"
    | Var s -> s
    | App (hd, tl) ->
        let tl =
          List.map
            (fun (lbl, v) ->
              (if lbl = "" then "" else lbl ^ " = ") ^ print_term v)
            tl
        in
        print_term hd ^ "(" ^ String.concat "," tl ^ ")"
    | Let _ | Seq _ -> assert false

let rec string_of_pat = function
  | PVar l -> String.concat "." l
  | PTuple l -> "(" ^ String.concat ", " (List.map string_of_pat l) ^ ")"

let rec free_vars_pat = function
  | PVar [] -> assert false
  | PVar [_] -> Vars.empty
  | PVar (x :: _) -> Vars.singleton x
  | PTuple l -> List.fold_left Vars.union Vars.empty (List.map free_vars_pat l)

let rec bound_vars_pat = function
  | PVar [] -> assert false
  | PVar [x] -> Vars.singleton x
  | PVar _ -> Vars.empty
  | PTuple l -> List.fold_left Vars.union Vars.empty (List.map bound_vars_pat l)

let rec free_vars tm =
  match tm.term with
    | Ground _ | Encoder _ -> Vars.empty
    | Var x -> Vars.singleton x
    | Tuple l ->
        List.fold_left (fun v a -> Vars.union v (free_vars a)) Vars.empty l
    | Null -> Vars.empty
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
    | Type.Tuple [] | Type.EVar _ -> true
    | _ -> false

(* TODO: what about functions with methods? *)
let is_fun t =
  match (Type.deref t).Type.descr with Type.Arrow _ -> true | _ -> false

let is_source t =
  match (Type.demeth t).Type.descr with
    | Type.Constr { Type.name = "source"; _ } -> true
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
      | Ground _ | Encoder _ -> v
      | Tuple l -> List.fold_left (fun a -> check a) v l
      | Null -> v
      | Cast (e, _) -> check v e
      | Meth (_, f, e) -> check (check v e) f
      | Invoke (e, _) -> check v e
      | Open (a, b) -> check (check v a) b
      | Seq (a, b) -> check ~toplevel (check v a) b
      | List l -> List.fold_left (fun x y -> check x y) v l
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
                    && not (can_ignore def.t || (toplevel && is_fun def.t))
                  then throw (Unused_variable (s, Option.get tm.t.Type.pos)))
              bvpat;
          Vars.union v mask
  in
  (* Unused free variables may remain *)
  ignore (check ~toplevel:true Vars.empty tm)

(** Values are untyped normal forms of terms. *)
module Value = struct
  module Methods = Map.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

  type t = { pos : Type.pos option; value : in_value; methods : t Methods.t }

  and env = (string * t) list

  (* Some values have to be lazy in the environment because of recursive functions. *)
  and lazy_env = (string * t Lazy.t) list

  and in_value =
    | Ground of Ground.t
    | Source of Source.source
    | Encoder of Encoder.format
    | List of t list
    | Tuple of t list
    | Null
    | Ref of t ref
    (* The first environment contains the parameters already passed to the
       function. Next parameters will be inserted between that and the second
       env which is part of the closure. *)
    | Fun of (string * string * t option) list * env * lazy_env * term
    (* For a foreign function only the arguments are visible, the closure
       doesn't capture anything in the environment. *)
    | FFI of (string * string * t option) list * env * (env -> t)

  let unit : in_value = Tuple []

  let string_of_float f =
    let s = string_of_float f in
    if s.[String.length s - 1] = '.' then s ^ "0" else s

  let split_meths e =
    (Methods.fold (fun m v l -> (m, v) :: l) e.methods [], e.value)

  let rec print_value v =
    let methods, value = split_meths v in
    let value_string () =
      match value with
        | Ground g -> Ground.to_string g
        | Source _ -> "<source>"
        | Encoder e -> Encoder.string_of_format e
        | List l -> "[" ^ String.concat ", " (List.map print_value l) ^ "]"
        | Ref a -> Printf.sprintf "ref(%s)" (print_value !a)
        | Tuple l -> "(" ^ String.concat ", " (List.map print_value l) ^ ")"
        | Null -> "null"
        | Fun ([], _, _, x) when is_ground x -> "{" ^ print_term x ^ "}"
        | Fun (l, _, _, x) when is_ground x ->
            let f (label, _, value) =
              match (label, value) with
                | "", None -> "_"
                | "", Some v -> Printf.sprintf "_=%s" (print_value v)
                | label, Some v -> Printf.sprintf "~%s=%s" label (print_value v)
                | label, None -> Printf.sprintf "~%s=_" label
            in
            let args = List.map f l in
            Printf.sprintf "fun (%s) -> %s" (String.concat "," args)
              (print_term x)
        | Fun _ | FFI _ -> "<fun>"
    in
    let methods_string () =
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map
              (fun (m, v) -> Printf.sprintf "%s = %s" m (print_value v))
              methods))
    in
    match (methods, value) with
      | [], _ -> value_string ()
      | _, Tuple [] -> methods_string ()
      | _, _ -> Printf.sprintf "%s.%s" (value_string ()) (methods_string ())

  (** Find a method in a value. *)
  let invoke x l =
    try Methods.find l x.methods
    with Not_found ->
      failwith ("Could not find method " ^ l ^ " of " ^ print_value x)

  (** Perform a sequence of invokes: invokes x [l1;l2;l3;...] is x.l1.l2.l3... *)
  let rec invokes x = function l :: ll -> invokes (invoke x l) ll | [] -> x

  let demeth v = { v with methods = Methods.empty }

  let remeth t u =
    { u with methods = Methods.union (fun _ v _ -> Some v) t.methods u.methods }
end
