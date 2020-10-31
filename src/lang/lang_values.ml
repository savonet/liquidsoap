(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

(** Values in the Liquidsoap language. *)

type pos = Lang_types.pos

(** An internal error. Those should not happen in theory... *)
exception Internal_error of (pos list * string)

(** A parsing error. *)
exception Parse_error of (pos * string)

(** Unsupported format *)
exception Unsupported_format of (pos * Encoder.format)

(** An error at runtime. *)
type runtime_error = { kind : string; msg : string option; pos : pos list }

exception Runtime_error of runtime_error

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

(* In a sense this could move to Lang_types, but I like to keep that
    part free of some specificities of liquidsoap, as much as possible. *)

module T = Lang_types

let ref_t ?pos ?level t =
  T.make ?pos ?level
    (T.Constr { T.name = "ref"; T.params = [(T.Invariant, t)] })

(** A frame kind type is a purely abstract type representing a
    frame kind. *)
let frame_kind_t ?pos ?level audio video midi =
  T.make ?pos ?level
    (T.Constr
       {
         T.name = "stream_kind";
         T.params =
           [(T.Covariant, audio); (T.Covariant, video); (T.Covariant, midi)];
       })

let kind_t ?pos ?level kind =
  let evar ?(constraints = []) () =
    T.fresh ~constraints
      ~pos:(match pos with None -> None | Some pos -> pos)
      ~level:(-1)
  in
  let mk_format f = T.make ?pos ?level (T.Ground (T.Format f)) in
  match kind with
    | `Any -> evar ()
    | `Internal -> evar ~constraints:[Lang_types.InternalMedia] ()
    | `Kind k ->
        T.make ?pos ?level
          (T.Constr
             {
               T.name = Frame_content.string_of_kind k;
               T.params = [(T.Covariant, evar ())];
             })
    | `Format f ->
        let k = Frame_content.kind f in
        T.make ?pos ?level
          (T.Constr
             {
               T.name = Frame_content.string_of_kind k;
               T.params = [(T.Covariant, mk_format f)];
             })

let of_frame_kind_t t =
  match (T.deref t).T.descr with
    | T.Constr
        {
          T.name = "stream_kind";
          T.params = [(_, audio); (_, video); (_, midi)];
        } ->
        { Frame.audio; video; midi }
    | T.EVar (_, _) ->
        let audio = kind_t `Any in
        let video = kind_t `Any in
        let midi = kind_t `Any in
        T.bind t (frame_kind_t audio video midi);
        { Frame.audio; video; midi }
    | _ -> assert false

(** Type of audio formats that can encode frame of a given kind. *)
let format_t ?pos ?level k =
  T.make ?pos ?level
    (T.Constr { T.name = "format"; T.params = [(T.Covariant, k)] })

(** Type of sources carrying frames of a given kind. *)
let source_t ?(active = false) ?pos ?level k =
  let name = if active then "active_source" else "source" in
  T.make ?pos ?level (T.Constr { T.name; T.params = [(T.Invariant, k)] })

let of_source_t t =
  match (T.deref t).T.descr with
    | T.Constr { T.name = "source"; T.params = [(_, t)] } -> t
    | T.Constr { T.name = "active_source"; T.params = [(_, t)] } -> t
    | _ -> assert false

let request_t ?pos ?level () = T.make ?pos ?level (T.Ground T.Request)

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
  type content = { descr : unit -> string; compare : t -> int; typ : T.ground }

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
  let to_type (v : t) = (find v).typ
  let compare (v : t) = (find v).compare

  type t +=
    | Bool of bool
    | Int of int
    | String of string
    | Float of float
    | Request of Request.t

  let () =
    register (function
      | Bool b ->
          let compare = function
            | Bool b' -> Stdlib.compare b b'
            | _ -> assert false
          in
          Some { descr = (fun () -> string_of_bool b); compare; typ = T.Bool }
      | Int i ->
          let compare = function
            | Int i' -> Stdlib.compare i i'
            | _ -> assert false
          in
          Some { descr = (fun () -> string_of_int i); compare; typ = T.Int }
      | String s ->
          let compare = function
            | String s' -> Stdlib.compare s s'
            | _ -> assert false
          in
          Some
            {
              descr = (fun () -> Printf.sprintf "%S" s);
              compare;
              typ = T.String;
            }
      | Float f ->
          let compare = function
            | Float f' -> Stdlib.compare f f'
            | _ -> assert false
          in
          Some { descr = (fun () -> string_of_float f); compare; typ = T.Float }
      | Request r ->
          let descr () = Printf.sprintf "<request(id=%d)>" (Request.get_id r) in
          let compare = function
            | Request r' ->
                Stdlib.compare (Request.get_id r) (Request.get_id r')
            | _ -> assert false
          in
          Some { descr; compare; typ = T.Request }
      | _ -> None)
end

module type GroundDef = sig
  type content

  val descr : content -> string
  val compare : content -> content -> int
  val typ : T.ground
end

module MkGround (D : GroundDef) = struct
  type Ground.t += Ground of D.content

  let () =
    Ground.register (function
      | Ground v ->
          let descr () = D.descr v in
          let compare = function
            | Ground v' -> D.compare v v'
            | _ -> assert false
          in
          Some { Ground.typ = D.typ; compare; descr }
      | _ -> None)
end

type term = { mutable t : T.t; term : in_term }

and let_t = {
  doc : Doc.item * (string * string) list;
  replace : bool;
  (* whether the definition replaces a previously existing one (keeping methods) *)
  pat : pattern;
  mutable gen : (int * T.constraints) list;
  def : term;
  body : term;
}

and in_term =
  | Ground of Ground.t
  | Encoder of Encoder.format
  | List of term list
  | Tuple of term list
  | Null
  | Meth of string * term * term
  | Invoke of term * string
  | Open of term * term
  | Let of let_t
  | Var of string
  | Seq of term * term
  | App of term * (string * term) list
  (* [fun ~l1:x1 .. ?li:(xi=defi) .. -> body] =
   * [Fun (V, [(l1,x1,None)..(li,xi,Some defi)..], body)]
   * The first component [V] is the list containing all
   * variables occurring in the function. It is used to
   * restrict the environment captured when a closure is
   * formed. *)
  | Fun of Vars.t * (string * string * T.t * term option) list * term
  | RFun of string * Vars.t * (string * string * T.t * term option) list * term

(* A recursive function, the first string is the name of the recursive
   variable. *)
and pattern =
  | PVar of string list  (** a field *)
  | PTuple of pattern list  (** a tuple *)

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
  match (T.demeth t).T.descr with
    | T.Tuple [] | T.Constr { T.name = "active_source"; _ } -> true
    | T.EVar _ -> true
    | _ -> false

(* TODO: what about functions with methods? *)
let is_fun t = match (T.deref t).T.descr with T.Arrow _ -> true | _ -> false

let is_source t =
  match (T.demeth t).T.descr with
    | T.Constr { T.name = "source"; _ } -> true
    | _ -> false

(** Check that all let-bound variables are used.
  * No check is performed for variable arguments.
  * This cannot be done at parse-time (as for the computatin of the
  * free variables of functions) because we need types, as well as
  * the ability to distinguish toplevel and inner let-in terms. *)

exception Unused_variable of (string * Lexing.position)

let check_unused ~throw ~lib tm =
  let rec check ?(toplevel = false) v tm =
    match tm.term with
      | Var s -> Vars.remove s v
      | Ground _ | Encoder _ -> v
      | Tuple l -> List.fold_left (fun a -> check a) v l
      | Null -> v
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
              (fun v -> function _, _, _, Some default -> check v default
                | _ -> v)
              v p
          in
          let masked, v =
            let v0 = v in
            List.fold_left
              (fun (masked, v) (_, var, _, _) ->
                if Vars.mem var v0 then (Vars.add var masked, v)
                else (masked, Vars.add var v))
              (Vars.empty, v) p
          in
          let v = check v body in
          (* Restore masked variables. The masking variables have been used but it
             does not count for the ones they masked. *)
          Vars.union masked v
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
                  (* There are exceptions: unit, active_source and functions when
                     at toplevel (sort of a lib situation...) *)
                  if
                    s <> "_"
                    && not (can_ignore def.t || (toplevel && is_fun def.t))
                  then (
                    let start_pos = fst (Option.get tm.t.T.pos) in
                    throw (Unused_variable (s, start_pos)) ))
              bvpat;
          Vars.union v mask
  in
  (* Unused free variables may remain *)
  ignore (check ~toplevel:true Vars.empty tm)

(** Values are untyped normal forms of terms. *)
module V = struct
  type value = { pos : T.pos option; value : in_value }

  and env = (string * value) list

  (* Some values have to be lazy in the environment because of recursive functions. *)
  and lazy_env = (string * value Lazy.t) list

  and in_value =
    | Ground of Ground.t
    | Source of Source.source
    | Encoder of Encoder.format
    | List of value list
    | Tuple of value list
    | Null
    (* TODO: It would be better to have a list of methods associated to each
       value than a constructor here. However, I am keeping as is for now because
       implementation is safer this way. *)
    | Meth of string * value * value
    | Ref of value ref
    (* The first environment contains the parameters already passed to the
       function. Next parameters will be inserted between that and the second
       env which is part of the closure. *)
    | Fun of (string * string * value option) list * env * lazy_env * term
    (* For a foreign function only the arguments are visible, the closure
       doesn't capture anything in the environment. *)
    | FFI of (string * string * value option) list * env * (env -> value)

  let unit : in_value = Tuple []

  let string_of_float f =
    let s = string_of_float f in
    if s.[String.length s - 1] = '.' then s ^ "0" else s

  let rec print_value v =
    match v.value with
      | Ground g -> Ground.to_string g
      | Source _ -> "<source>"
      | Encoder e -> Encoder.string_of_format e
      | List l -> "[" ^ String.concat ", " (List.map print_value l) ^ "]"
      | Ref a -> Printf.sprintf "ref(%s)" (print_value !a)
      | Tuple l -> "(" ^ String.concat ", " (List.map print_value l) ^ ")"
      | Null -> "null"
      | Meth (l, v, e) when Lazy.force debug ->
          print_value e ^ ".{" ^ l ^ "=" ^ print_value v ^ "}"
      | Meth _ ->
          let rec split e =
            match e.value with
              | Meth (l, v, e) ->
                  let m, e = split e in
                  ((l, v) :: m, e)
              | _ -> ([], e)
          in
          let m, e = split v in
          let m =
            List.rev m
            |> List.map (fun (l, v) -> l ^ " = " ^ print_value v)
            |> String.concat ", "
          in
          let e =
            match e.value with Tuple [] -> "" | _ -> print_value e ^ "."
          in
          e ^ "{" ^ m ^ "}"
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

  (** Find a method in a value. *)
  let rec invoke x l =
    match x.value with
      | Meth (l', y, _) when l' = l -> y
      | Meth (_, _, x) -> invoke x l
      | _ -> failwith ("Could not find method " ^ l ^ " of " ^ print_value x)

  (** Perform a sequence of invokes: invokes x [l1;l2;l3;...] is x.l1.l2.l3... *)
  let rec invokes x = function l :: ll -> invokes (invoke x l) ll | [] -> x

  let rec demeth v = match v.value with Meth (_, _, v) -> demeth v | _ -> v

  let rec remeth t u =
    match t.value with
      | Meth (l, v, t) -> { t with value = Meth (l, v, remeth t u) }
      | _ -> u
end

(** {2 Built-in values and toplevel definitions} *)

let builtins : (((int * T.constraints) list * T.t) * V.value) Plug.plug =
  Plug.create ~duplicates:false ~doc:"scripting values" "scripting values"

(* Environment for builtins. *)
let builtins_env : (string * (T.scheme * V.value)) list ref = ref []
let default_environment () = !builtins_env

let default_typing_environment () =
  List.map (fun (x, (t, _)) -> (x, t)) !builtins_env

let add_builtin ?(override = false) ?(register = true) ?doc name ((g, t), v) =
  if register then builtins#register ?doc (String.concat "." name) ((g, t), v);
  match name with
    | [name] ->
        (* Don't allow overriding builtins. *)
        if (not override) && List.mem_assoc name !builtins_env then
          failwith ("Trying to override builtin " ^ name);
        builtins_env := (name, ((g, t), v)) :: !builtins_env
    | x :: ll ->
        let (g0, t0), xv =
          try List.assoc x !builtins_env
          with Not_found -> failwith ("Could not find builtin variable " ^ x)
        in
        (* x.l1.l2.l3 = v means
           x = (x where l1 = (x.l1 where l2 = (x.l1.l2 where l3 = v)))
        *)
        (* Inductive step: we compute the new type scheme and value of
           x.l1...li. The variable prefix contains [li; ...; l1] and the second
           argument is [li+1; ...; ln]. *)
        let rec aux prefix = function
          | l :: ll ->
              (* Previous type scheme for x.l1...li. *)
              let vg, vt = T.invokes t0 (List.rev prefix) in
              (* Previous value of x.l1...li.  *)
              let v = V.invokes xv (List.rev prefix) in
              (* Updated value of x.l1...li+1. *)
              let (lvg, lvt), lv = aux (l :: prefix) ll in
              (* Updated type for x.l1...li, obtained by changing the type of
                 the field li+1. *)
              let t = T.make ~pos:t.T.pos (T.Meth (l, (lvg, lvt), vt)) in
              (* Update value for x.l1...li. *)
              let value = V.Meth (l, lv, v) in
              ((vg, t), { V.pos = v.V.pos; value })
          | [] -> ((g, t), v)
        in
        let (g, t), v = aux [] ll in
        assert (g = []);
        builtins_env := (x, ((g0, t), v)) :: !builtins_env
    | [] -> assert false

let has_builtin name = builtins#is_registered name
let get_builtin name = builtins#get name

(** Declare a module. *)
let add_module name =
  (* Ensure that it does not already exist. *)
  ( match name with
    | [] -> assert false
    | [x] ->
        if List.mem_assoc x !builtins_env then
          failwith ("Module " ^ String.concat "." name ^ " already declared")
    | x :: mm -> (
        let mm = List.rev mm in
        let l = List.hd mm in
        let mm = List.rev (List.tl mm) in
        let e =
          try V.invokes (snd (List.assoc x !builtins_env)) mm
          with _ ->
            failwith
              ("Could not find the parent module of " ^ String.concat "." name)
        in
        try
          ignore (V.invoke e l);
          failwith ("Module " ^ String.concat "." name ^ " already exists")
        with _ -> () ) );
  add_builtin ~register:false name
    (([], T.make T.unit), { V.pos = None; value = V.unit })

(* Builtins are only used for documentation now. *)
let builtins = (builtins :> Doc.item)

(** {2 Type checking/inference} *)

let ( <: ) = T.( <: )
let ( >: ) = T.( >: )

let rec value_restriction t =
  match t.term with
    | Var _ -> true
    | Fun _ -> true
    | RFun _ -> true
    | Null -> true
    | List l | Tuple l -> List.for_all value_restriction l
    | Meth (_, t, u) -> value_restriction t && value_restriction u
    (* | Invoke (t, _) -> value_restriction t *)
    | Ground _ -> true
    | _ -> false

exception Unbound of T.pos option * string
exception Ignored of term

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
  * passed to the function [f] because the latter has no label [lbl].
  * The [first] information tells whether [lbl=x] is the first parameter with
  * label [lbl] in the considered application, which makes the message a bit
  * more helpful. *)
exception No_label of term * string * bool * term

(** A simple mechanism for delaying printing toplevel tasks
  * as late as possible, to avoid seeing too many unknown variables. *)
let add_task, pop_tasks =
  let q = Queue.create () in
  ( (fun f -> Queue.add f q),
    fun () ->
      try
        while true do
          (Queue.take q) ()
        done
      with Queue.Empty -> () )

(** Generate a type with fresh variables for a patten. *)
let rec type_of_pat ~level ~pos = function
  | PVar x ->
      let a = T.fresh_evar ~level ~pos in
      ([(x, a)], a)
  | PTuple l ->
      let env, l =
        List.fold_left
          (fun (env, l) p ->
            let env', a = type_of_pat ~level ~pos p in
            (env' @ env, a :: l))
          ([], []) l
      in
      let l = List.rev l in
      (env, T.make ~level ~pos (T.Tuple l))

(* Type-check an expression.
 * [level] should be the sum of the lengths of [env] and [builtins],
 * that is the size of the typing context, that is the number of surrounding
 * abstractions. *)
let rec check ?(print_toplevel = false) ~throw ~level ~(env : T.env) e =
  (* The role of this function is not only to type-check but also to assign
   * meaningful levels to type variables, and unify the types of
   * all occurrences of the same variable, since the parser does not do it. *)
  assert (e.t.T.level = -1);
  e.t.T.level <- level;

  (* The toplevel position of the (un-dereferenced) type
   * is the actual parsing position of the value.
   * When we synthesize a type against which the type of the term is unified,
   * we have to set the position information in order not to loose it. *)
  let pos = e.t.T.pos in
  let mk t = T.make ~level ~pos t in
  let mkg t = mk (T.Ground t) in
  let check_fun ~proto ~env e body =
    let base_check = check ~throw ~level ~env in
    let proto_t, env, level =
      List.fold_left
        (fun (p, env, level) -> function
          | lbl, var, kind, None ->
              if Lazy.force debug then
                Printf.eprintf "Assigning level %d to %s (%s).\n" level var
                  (T.print kind);
              kind.T.level <- level;
              ((false, lbl, kind) :: p, (var, ([], kind)) :: env, level + 1)
          | lbl, var, kind, Some v ->
              if Lazy.force debug then
                Printf.eprintf "Assigning level %d to %s (%s).\n" level var
                  (T.print kind);
              kind.T.level <- level;
              base_check v;
              v.t <: kind;
              ((true, lbl, kind) :: p, (var, ([], kind)) :: env, level + 1))
        ([], env, level) proto
    in
    let proto_t = List.rev proto_t in
    check ~throw ~level ~env body;
    e.t >: mk (T.Arrow (proto_t, body.t))
  in
  match e.term with
    | Ground g -> e.t >: mkg (Ground.to_type g)
    | Encoder f -> e.t >: type_of_format ~pos:e.t.T.pos ~level f
    | List l ->
        List.iter (fun x -> check ~throw ~level ~env x) l;
        let t =
          List.fold_left
            (fun t e -> T.min_type ~pos:e.t.T.pos ~level t e.t)
            (T.fresh_evar ~level ~pos) l
        in
        List.iter (fun e -> e.t <: t) l;
        e.t >: mk (T.List t)
    | Tuple l ->
        List.iter (fun a -> check ~throw ~level ~env a) l;
        e.t >: mk (T.Tuple (List.map (fun a -> a.t) l))
    | Null -> e.t >: mk (T.Nullable (T.fresh_evar ~level ~pos))
    | Meth (l, a, b) ->
        check ~throw ~level ~env a;
        check ~throw ~level ~env b;
        e.t >: mk (T.Meth (l, (T.generalizable ~level a.t, a.t), b.t))
    | Invoke (a, l) ->
        check ~throw ~level ~env a;
        let rec aux t =
          match (T.deref t).T.descr with
            | T.Meth (l', (generalized, b), c) ->
                if l = l' then T.instantiate ~level ~generalized b else aux c
            | _ ->
                (* We did not find the method, the type we will infer is not the
                   most general one (no generalization), but this is safe and
                   enough for records. *)
                let x = T.fresh_evar ~level ~pos in
                let y = T.fresh_evar ~level ~pos in
                a.t <: mk (T.Meth (l, ([], x), y));
                x
        in
        e.t >: aux a.t
    | Open (a, b) ->
        check ~throw ~level ~env a;
        a.t <: mk T.unit;
        let rec aux env t =
          match (T.deref t).T.descr with
            | T.Meth (l, (g, u), t) -> aux ((l, (g, u)) :: env) t
            | _ -> env
        in
        let env = aux env a.t in
        check ~throw ~level ~env b;
        e.t >: b.t
    | Seq (a, b) ->
        check ~throw ~env ~level a;
        if not (can_ignore a.t) then throw (Ignored a);
        check ~print_toplevel ~throw ~level ~env b;
        e.t >: b.t
    | App (a, l) -> (
        check ~throw ~level ~env a;
        List.iter (fun (_, b) -> check ~throw ~env ~level b) l;

        (* If [a] is known to have a function type, manually dig through
         * it for better error messages. Otherwise generate its type
         * and unify -- in that case the optionality can't be guessed
         * and mandatory is the default. *)
        match (T.demeth a.t).T.descr with
          | T.Arrow (ap, t) ->
              (* Find in l the first arg labeled lbl,
               * return it together with the remaining of the list. *)
              let get_arg lbl l =
                let rec aux acc = function
                  | [] -> None
                  | (o, lbl', t) :: l ->
                      if lbl = lbl' then Some (o, t, List.rev_append acc l)
                      else aux ((o, lbl', t) :: acc) l
                in
                aux [] l
              in
              let _, ap =
                (* Remove the applied parameters,
                 * check their types on the fly. *)
                List.fold_left
                  (fun (already, ap) (lbl, v) ->
                    match get_arg lbl ap with
                      | None ->
                          let first = not (List.mem lbl already) in
                          raise (No_label (a, lbl, first, v))
                      | Some (_, t, ap') ->
                          v.t <: t;
                          (lbl :: already, ap'))
                  ([], ap) l
              in
              (* See if any mandatory argument remains, check the return type. *)
              if List.for_all (fun (o, _, _) -> o) ap then e.t >: t
              else e.t >: T.make ~level ~pos:None (T.Arrow (ap, t))
          | _ ->
              let p = List.map (fun (lbl, b) -> (false, lbl, b.t)) l in
              a.t <: T.make ~level ~pos:None (T.Arrow (p, e.t)) )
    | Fun (_, proto, body) -> check_fun ~proto ~env e body
    | RFun (x, _, proto, body) ->
        let env = (x, ([], e.t)) :: env in
        check_fun ~proto ~env e body
    | Var var ->
        let generalized, orig =
          try List.assoc var env
          with Not_found -> raise (Unbound (e.t.T.pos, var))
        in
        e.t >: T.instantiate ~level ~generalized orig;
        if Lazy.force debug then
          Printf.eprintf "Instantiate %s[%d] : %s becomes %s\n" var
            (T.deref e.t).T.level (T.print orig) (T.print e.t)
    | Let ({ pat; replace; def; body; _ } as l) ->
        check ~throw ~level ~env def;
        let generalized =
          if value_restriction def then (
            let f x t =
              let x' =
                T.filter_vars
                  (function
                    | { T.descr = T.EVar (i, _); level = l; _ } ->
                        (not (List.mem_assoc i x)) && l >= level
                    | _ -> assert false)
                  t
              in
              x' @ x
            in
            f [] def.t )
          else []
        in
        let penv, pa = type_of_pat ~level ~pos pat in
        def.t <: pa;
        let penv =
          List.map
            (fun (ll, a) ->
              match ll with
                | [] -> assert false
                | [x] ->
                    let a =
                      if replace then T.remeth (snd (List.assoc x env)) a else a
                    in
                    (x, (generalized, a))
                | l :: ll -> (
                    try
                      let g, t = List.assoc l env in
                      let a =
                        (* If we are replacing the value, we keep the previous methods. *)
                        if replace then T.remeth (snd (T.invokes t ll)) a else a
                      in
                      (l, (g, T.meths ~pos ~level ll (generalized, a) t))
                    with Not_found ->
                      raise (Unbound (pos, String.concat "." (l :: ll))) ))
            penv
        in
        let env = penv @ env in
        l.gen <- generalized;
        if print_toplevel then
          add_task (fun () ->
              Format.printf "@[<2>%s :@ %a@]@."
                (let name = string_of_pat pat in
                 let l = String.length name and max = 5 in
                 if l >= max then name else name ^ String.make (max - l) ' ')
                (T.pp_type_generalized generalized)
                def.t);
        check ~print_toplevel ~throw ~level:(level + 1) ~env body;
        e.t >: body.t

(* The simple definition for external use. *)
let check ?(ignored = false) ~throw e =
  let print_toplevel = !Configure.display_types in
  try
    let env = default_typing_environment () in
    check ~print_toplevel ~throw ~level:(List.length env) ~env e;
    if print_toplevel && (T.deref e.t).T.descr <> T.unit then
      add_task (fun () -> Format.printf "@[<2>-     :@ %a@]@." T.pp_type e.t);
    if ignored && not (can_ignore e.t) then throw (Ignored e);
    pop_tasks ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    pop_tasks ();
    Printexc.raise_with_backtrace e bt

(** {2 Computations} *)

(** [remove_first f l] removes the first element [e] of [l] such that [f e],
  * and returns [e,l'] where [l'] is the list without [e].
  * Asserts that there is such an element. *)
let remove_first filter =
  let rec aux acc = function
    | [] -> assert false
    | hd :: tl ->
        if filter hd then (hd, List.rev_append acc tl) else aux (hd :: acc) tl
  in
  aux []

let lookup (env : V.lazy_env) var = Lazy.force (List.assoc var env)

let eval_pat pat v =
  let rec aux env pat v =
    match (pat, v) with
      | PVar x, v -> (x, v) :: env
      | PTuple pl, { V.value = V.Tuple l } -> List.fold_left2 aux env pl l
      | _ -> assert false
  in
  aux [] pat v

let rec eval ~env tm =
  let env = (env : V.lazy_env) in
  let prepare_fun fv p env =
    (* Unlike OCaml we always evaluate default values, and we do that early. I
       think the only reason is homogeneity with FFI, which are declared with
       values as defaults. *)
    let p =
      List.map
        (function
          | lbl, var, _, Some v -> (lbl, var, Some (eval ~env v))
          | lbl, var, _, None -> (lbl, var, None))
        p
    in
    (* Keep only once the variables we might use in the environment. *)
    let env =
      let fv = ref fv in
      let mem x =
        if Vars.mem x !fv then (
          fv := Vars.remove x !fv;
          true )
        else false
      in
      List.filter (fun (x, _) -> mem x) env
    in
    (p, env)
  in
  let mk v =
    ( (* Ensure that the kind computed at runtime for sources will agree with
         the typing. *)
      match (T.deref tm.t).T.descr with
      | T.Constr { T.name = "source"; params = [(T.Invariant, k)] } -> (
          let k =
            match of_frame_kind_t k with
              | {
               Frame.audio = { T.descr = T.Ground (T.Format audio) };
               video = { T.descr = T.Ground (T.Format video) };
               midi = { T.descr = T.Ground (T.Format midi) };
              } ->
                  Source.Kind.of_kind
                    {
                      Frame.audio = `Format audio;
                      video = `Format video;
                      midi = `Format midi;
                    }
              | _ -> assert false
          in
          match v with
            | V.Source s -> Source.Kind.unify s#kind k
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.T.pos,
                       "term has type source but is not a source: "
                       ^ V.print_value { V.pos = tm.t.T.pos; V.value = v } )) )
      | _ -> () );
    { V.pos = tm.t.T.pos; V.value = v }
  in
  match tm.term with
    | Ground g -> mk (V.Ground g)
    | Encoder x -> mk (V.Encoder x)
    | List l -> mk (V.List (List.map (eval ~env) l))
    | Tuple l -> mk (V.Tuple (List.map (fun a -> eval ~env a) l))
    | Null -> mk V.Null
    | Meth (l, u, v) -> mk (V.Meth (l, eval ~env u, eval ~env v))
    | Invoke (t, l) ->
        let rec aux t =
          match t.V.value with
            | V.Meth (l', t, _) when l = l' -> t
            | V.Meth (_, _, t) -> aux t
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.T.pos,
                       "invoked method " ^ l ^ " not found" ))
        in
        aux (eval ~env t)
    | Open (t, u) ->
        let t = eval ~env t in
        let rec aux env t =
          match t.V.value with
            | V.Meth (l, v, t) -> aux ((l, Lazy.from_val v) :: env) t
            | V.Tuple [] -> env
            | _ -> assert false
        in
        let env = aux env t in
        eval ~env u
    | Let { pat; replace; def = v; body = b; _ } ->
        let v = eval ~env v in
        let penv =
          List.map
            (fun (ll, v) ->
              match ll with
                | [] -> assert false
                | [x] ->
                    let v () =
                      if replace then V.remeth (Lazy.force (List.assoc x env)) v
                      else v
                    in
                    (x, Lazy.from_fun v)
                | l :: ll ->
                    (* Add method ll with value v to t *)
                    let rec meths ll v t =
                      let mk ~pos value = { V.pos; value } in
                      match ll with
                        | [] -> assert false
                        | [l] -> mk ~pos:tm.t.T.pos (V.Meth (l, v, t))
                        | l :: ll ->
                            mk ~pos:t.V.pos
                              (V.Meth (l, meths ll v (V.invoke t l), t))
                    in
                    let v () =
                      let t = Lazy.force (List.assoc l env) in
                      let v =
                        (* When replacing, keep previous methods. *)
                        if replace then V.remeth (V.invokes t ll) v else v
                      in
                      meths ll v t
                    in
                    (l, Lazy.from_fun v))
            (eval_pat pat v)
        in
        let env = penv @ env in
        eval ~env b
    | Fun (fv, p, body) ->
        let p, env = prepare_fun fv p env in
        mk (V.Fun (p, [], env, body))
    | RFun (x, fv, p, body) ->
        let p, env = prepare_fun fv p env in
        let rec v () =
          let env = (x, Lazy.from_fun v) :: env in
          { V.pos = tm.t.T.pos; value = V.Fun (p, [], env, body) }
        in
        v ()
    | Var var -> lookup env var
    | Seq (a, b) ->
        ignore (eval ~env a);
        eval ~env b
    | App (f, l) ->
        let ans () =
          apply (eval ~env f) (List.map (fun (l, t) -> (l, eval ~env t)) l)
        in
        if !profile then (
          match f.term with
            | Var fname -> Profiler.time fname ans ()
            | _ -> ans () )
        else ans ()

and apply f l =
  let rec pos = function
    | [(_, v)] -> (
        match (f.V.pos, v.V.pos) with
          | Some (p, _), Some (_, q) -> Some (p, q)
          | Some pos, None -> Some pos
          | None, Some pos -> Some pos
          | None, None -> None )
    | _ :: l -> pos l
    | [] -> f.V.pos
  in
  (* Position of the whole application. *)
  let pos = pos l in
  let pos_f = f.V.pos in
  let mk ~pos v = { pos; V.value = v } in
  (* Extract the components of the function, whether it's explicit or foreign,
     together with a rewrapping function for creating a closure in case of
     partial application. *)
  let p, pe, f, rewrap =
    match (V.demeth f).V.value with
      | V.Fun (p, pe, e, body) ->
          ( p,
            pe,
            (fun pe ->
              let pe = List.map (fun (x, gv) -> (x, Lazy.from_val gv)) pe in
              eval ~env:(List.rev_append pe e) body),
            fun p pe -> mk ~pos:pos_f (V.Fun (p, pe, e, body)) )
      | V.FFI (p, pe, f) ->
          ( p,
            pe,
            (fun pe -> f (List.rev pe)),
            fun p pe -> mk ~pos:pos_f (V.FFI (p, pe, f)) )
      | _ -> assert false
  in
  (* Record error positions. *)
  let f pe =
    try f pe with
      | Runtime_error err ->
          raise (Runtime_error { err with pos = Option.to_list pos @ err.pos })
      | Internal_error (poss, e) ->
          raise (Internal_error (Option.to_list pos @ poss, e))
  in
  let pe, p =
    List.fold_left
      (fun (pe, p) (lbl, v) ->
        let (_, var, _), p = remove_first (fun (l, _, _) -> l = lbl) p in
        ((var, v) :: pe, p))
      (pe, p) l
  in
  if List.exists (fun (_, _, x) -> x = None) p then
    (* Partial application. *)
    rewrap p pe
  else (
    let pe =
      List.fold_left
        (fun pe (_, var, v) ->
          ( var,
            (* Set the position information on FFI's default values. Cf. r5008:
               if an Invalid_value is raised on a default value, which happens
               with the mount/name params of output.icecast.*, the printing of
               the error should succeed at getting a position information. *)
            let v = Option.get v in
            { v with V.pos } )
          :: pe)
        pe p
    in
    let v = f pe in
    (* Similarly here, the result of an FFI call should have some position
       information. For example, if we build a fallible source and pass it to an
       operator that expects an infallible one, an error is issued about that
       FFI-made value and a position is needed. *)
    { v with V.pos } )

let eval ?env tm =
  let env = match env with Some env -> env | None -> default_environment () in
  let env = List.map (fun (x, (_, v)) -> (x, Lazy.from_val v)) env in
  eval ~env tm

(** Add toplevel definitions to [builtins] so they can be looked during the
    evaluation of the next scripts. Also try to generate a structured
    documentation from the source code. *)
let toplevel_add (doc, params) pat ~t v =
  let generalized, t = t in
  let rec ptypes t =
    match (T.deref t).T.descr with
      | T.Arrow (p, _) -> p
      | T.Meth (_, _, t) -> ptypes t
      | _ -> []
  in
  let ptypes = ptypes t in
  let rec pvalues v =
    match v.V.value with
      | V.Fun (p, _, _, _) -> List.map (fun (l, _, o) -> (l, o)) p
      | V.Meth (_, _, v) -> pvalues v
      | _ -> []
  in
  let pvalues = pvalues v in
  let params, _ =
    List.fold_left
      (fun (params, pvalues) (_, label, t) ->
        let descr, params =
          try (List.assoc label params, List.remove_assoc label params)
          with Not_found -> ("", params)
        in
        let default, pvalues =
          try
            (`Known (List.assoc label pvalues), List.remove_assoc label pvalues)
          with Not_found -> (`Unknown, pvalues)
        in
        let item = Doc.trivial (if descr = "" then "(no doc)" else descr) in
        item#add_subsection "type" (T.doc_of_type ~generalized t);
        item#add_subsection "default"
          (Doc.trivial
             ( match default with
               | `Unknown -> "???"
               | `Known (Some v) -> V.print_value v
               | `Known None -> "None" ));
        doc#add_subsection (if label = "" then "(unlabeled)" else label) item;
        (params, pvalues))
      (params, pvalues) ptypes
  in
  List.iter
    (fun (s, _) ->
      Printf.eprintf "WARNING: Unused @param %S for %s %s\n" s
        (string_of_pat pat) (T.print_pos_opt v.V.pos))
    params;
  doc#add_subsection "_type" (T.doc_of_type ~generalized t);
  List.iter
    (fun (x, v) -> add_builtin ~override:true ~doc x ((generalized, t), v))
    (eval_pat pat v)

let rec eval_toplevel ?(interactive = false) t =
  match t.term with
    | Let { doc = comment; gen = generalized; replace; pat; def; body } ->
        let def_t, def =
          if not replace then (def.t, eval def)
          else (
            match pat with
              | PVar [] -> assert false
              | PVar (x :: l) ->
                  let old_t, old = List.assoc x (default_environment ()) in
                  let old_t = snd old_t in
                  let old_t = snd (T.invokes old_t l) in
                  let old = V.invokes old l in
                  (T.remeth old_t def.t, V.remeth old (eval def))
              | PTuple _ ->
                  failwith "TODO: cannot replace toplevel tuples for now" )
        in
        toplevel_add comment pat ~t:(generalized, def_t) def;
        if Lazy.force debug then
          Printf.eprintf "Added toplevel %s : %s\n%!" (string_of_pat pat)
            (T.print ~generalized def_t);
        let var = string_of_pat pat in
        if interactive && var <> "_" then
          Format.printf "@[<2>%s :@ %a =@ %s@]@." var
            (T.pp_type_generalized generalized)
            def_t (V.print_value def);
        eval_toplevel ~interactive body
    | Seq (a, b) ->
        ignore
          (let v = eval_toplevel a in
           if v.V.pos = None then { v with V.pos = a.t.T.pos } else v);
        eval_toplevel ~interactive b
    | _ ->
        let v = eval t in
        if interactive && t.term <> unit then
          Format.printf "- : %a = %s@." T.pp_type t.t (V.print_value v);
        v
