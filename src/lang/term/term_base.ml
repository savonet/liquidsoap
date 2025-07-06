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

(** Terms and values in the Liquidsoap language. *)

include Runtime_term

type encoder = t Runtime_term.encoder
type encoder_params = t Runtime_term.encoder_params
type parsed_pos = Lexing.position * Lexing.position

(** An internal error. Those should not happen in theory... *)
exception Internal_error of (Pos.t list * string)

(** A parsing error. *)
exception Parse_error of (parsed_pos * string)

(** Unsupported encoder *)
exception Unsupported_encoder of (Pos.t option * string)

let () =
  Printexc.register_printer (function
    | Internal_error (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Internal_error %s: %s"
             (Pos.List.to_string pos) e)
    | Parse_error (pos, e) ->
        Some
          (Printf.sprintf "Term_base.Parse_error %s: %s"
             Pos.(to_string (of_lexing_pos pos))
             e)
    | Unsupported_encoder (pos, e) ->
        Some
          (Printf.sprintf "Lang_values.Unsupported_encoder at %s: %s"
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

module Custom = Term_custom

let unit = `Tuple []

(* Only used for printing very simple functions. *)
let rec is_ground x =
  match x.term with
    | `List l | `Tuple l -> List.for_all is_ground l
    | `Null | `Int _ | `Float _ | `String _ | `Bool _ -> true
    | _ -> false

let string_of_pat = function
  | `PVar l -> String.concat "." l
  | `PTuple l -> "(" ^ String.concat ", " l ^ ")"

(** String representation of terms, (almost) assuming they are in normal form.
*)

let rec to_string (v : t) =
  let to_base_string (v : t) =
    match v.term with
      | `Cache_env _ -> "<cache_env>"
      | `Custom c -> Custom.to_string c
      | `Int i ->
          if has_flag v Flags.octal_int then Printf.sprintf "0o%o" i
          else if has_flag v Flags.hex_int then Printf.sprintf "0x%x" i
          else string_of_int i
      | `Float f -> Utils.string_of_float f
      | `Bool b -> string_of_bool b
      | `String s -> Lang_string.quote_string s
      | `Encoder e ->
          let rec aux (e, p) =
            let p =
              p
              |> List.map (function
                   | `Anonymous s -> s
                   | `Encoder e -> aux e
                   | `Labelled (l, v) -> l ^ "=" ^ to_string v)
              |> String.concat ", "
            in
            "%" ^ e ^ "(" ^ p ^ ")"
          in
          aux e
      | `List l -> "[" ^ String.concat ", " (List.map to_string l) ^ "]"
      | `Tuple l -> "(" ^ String.concat ", " (List.map to_string l) ^ ")"
      | `Null -> "null"
      | `Hide (tm, l) ->
          "{"
          ^ String.concat ", " (List.map (Printf.sprintf "%s = _") l)
          ^ ", ..." ^ to_string tm ^ "}"
      | `Cast { cast; typ } ->
          "(" ^ to_string cast ^ " : " ^ Type.to_string typ ^ ")"
      | `Invoke { invoked = e; meth = l; invoke_default } -> (
          match invoke_default with
            | None -> to_string e ^ "." ^ l
            | Some v -> "(" ^ to_string e ^ "." ^ l ^ " ?? " ^ to_string v ^ ")"
          )
      | `Open (m, e) -> "open " ^ to_string m ^ " " ^ to_string e
      | `Fun { name = None; arguments = []; body = v } when is_ground v ->
          "{" ^ to_string v ^ "}"
      | `Fun _ -> "<fun>"
      | `Var s -> s
      | `App (hd, tl) ->
          let tl =
            List.map
              (fun (lbl, v) ->
                (if lbl = "" then "" else lbl ^ " = ") ^ to_string v)
              tl
          in
          to_string hd ^ "(" ^ String.concat "," tl ^ ")"
      (* | Let _ | Seq _ -> assert false *)
      | `Let { pat; def; body } ->
          Printf.sprintf "let %s = %s in %s" (string_of_pat pat) (to_string def)
            (to_string body)
      | `Seq (e, e') -> to_string e ^ "; " ^ to_string e'
  in
  let term = to_base_string v in
  if Methods.is_empty v.methods then term
  else (
    let methods = Methods.bindings v.methods in
    (if v.term = `Tuple [] then "" else term ^ ".")
    ^ "{"
    ^ String.concat ", "
        (List.map (fun (l, meth_term) -> l ^ "=" ^ to_string meth_term) methods)
    ^ "}")

(** Create a new value. *)
let id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

let make ?pos ?t ?(flags = Flags.empty) ?(methods = Methods.empty) e =
  let t = match t with Some t -> t | None -> Type.var ?pos () in
  { t; term = e; methods; flags }

let rec free_vars_pat = function
  | `PVar [] -> assert false
  | `PVar [_] -> Vars.empty
  | `PVar (x :: _) -> Vars.singleton x
  | `PTuple l -> List.fold_left Vars.union Vars.empty (List.map free_vars_pat l)
  | `PList (l, spread, l') ->
      List.fold_left Vars.union Vars.empty
        (List.map free_vars_pat
           (l @ (match spread with None -> [] | Some v -> [`PVar [v]]) @ l'))
  | `PMeth (pat, l) ->
      List.fold_left Vars.union
        (match pat with None -> Vars.empty | Some pat -> free_vars_pat pat)
        (List.map free_vars_pat
           (List.fold_left
              (fun cur (lbl, pat) ->
                [`PVar [lbl]]
                @ (match pat with
                    | `None | `Nullable -> []
                    | `Pattern pat -> [pat])
                @ cur)
              [] l))

let bound_vars_pat = function
  | `PVar [] -> assert false
  | `PVar [x] -> Vars.singleton x
  | `PVar _ -> Vars.empty
  | `PTuple l -> Vars.of_list l

let rec free_term_vars tm =
  let root_free_vars = function
    | `Cache_env _ | `Int _ | `Float _ | `String _ | `Bool _ | `Custom _ ->
        Vars.empty
    | `Var x -> Vars.singleton x
    | `Tuple l ->
        List.fold_left (fun v a -> Vars.union v (free_vars a)) Vars.empty l
    | `Null -> Vars.empty
    | `Encoder e ->
        let rec enc (_, p) =
          List.fold_left
            (fun v t ->
              match t with
                | `Anonymous _ -> v
                | `Labelled (_, t) -> Vars.union v (free_vars t)
                | `Encoder e -> Vars.union v (enc e))
            Vars.empty p
        in
        enc e
    | `Cast { cast = e } -> free_vars e
    | `Seq (a, b) -> Vars.union (free_vars a) (free_vars b)
    | `Hide (tm, l) ->
        free_vars
          {
            tm with
            methods = Methods.filter (fun n _ -> not (List.mem n l)) tm.methods;
          }
    | `Invoke { invoked = e; invoke_default } ->
        Vars.union (free_vars e)
          (match invoke_default with
            | None -> Vars.empty
            | Some d -> free_vars d)
    | `Open (a, b) -> Vars.union (free_vars a) (free_vars b)
    | `List l ->
        List.fold_left (fun v t -> Vars.union v (free_vars t)) Vars.empty l
    | `App (hd, l) ->
        List.fold_left
          (fun v (_, t) -> Vars.union v (free_vars t))
          (free_vars hd) l
    | `Fun p -> free_fun_vars p
    | `Let l ->
        Vars.union
          (match l.pat with
            | `PVar (x :: _ :: _) -> Vars.singleton x
            | _ -> Vars.empty)
          (Vars.union (free_vars l.def)
             (Vars.diff (free_vars l.body) (bound_vars_pat l.pat)))
  in
  Methods.fold
    (fun _ meth_term fv -> Vars.union fv (free_vars meth_term))
    tm.methods (root_free_vars tm.term)

and free_fun_vars = function
  | { free_vars = Some fv } -> fv
  | { arguments; body } as p ->
      let bound =
        List.map
          (fun { label; as_variable } ->
            Option.value ~default:label as_variable)
          arguments
      in
      let fv =
        List.fold_left
          (fun fv -> function
            | { default = Some d } -> Vars.union fv (free_vars d) | _ -> fv)
          Vars.empty arguments
      in
      let fv = Vars.union fv (free_vars ~bound body) in
      p.free_vars <- Some fv;
      fv

and free_vars ?(bound = []) body : Vars.t =
  Vars.diff (free_term_vars body) (Vars.of_list bound)

(** Values which can be ignored (and will thus not raise a warning if ignored).
*)
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
    passed to the function [f] because the latter has no label [lbl]. The
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

exception Deprecated of (string * Pos.t)

let check_unused ~throw ~lib tm =
  let rec check ?(toplevel = false) v tm =
    let v =
      Methods.fold (fun _ meth_term e -> check e meth_term) tm.methods v
    in
    match tm.term with
      | `Var s -> Vars.remove s v
      | `Cache_env _ | `Int _ | `Float _ | `String _ | `Bool _ -> v
      | `Custom _ -> v
      | `Tuple l -> List.fold_left (fun a -> check a) v l
      | `Null -> v
      | `Hide (tm, l) ->
          check v
            {
              tm with
              methods =
                Methods.filter (fun n _ -> not (List.mem n l)) tm.methods;
            }
      | `Cast { cast = e } -> check v e
      | `Invoke { invoked = e } -> check v e
      | `Open (a, b) -> check (check v a) b
      | `Seq (a, b) -> check ~toplevel (check v a) b
      | `List l -> List.fold_left (fun x y -> check x y) v l
      | `Encoder e ->
          let rec enc v (_, p) =
            List.fold_left
              (fun v t ->
                match t with
                  | `Anonymous _ -> v
                  | `Labelled (_, t) -> check v t
                  | `Encoder e -> enc v e)
              v p
          in
          enc v e
      | `App (hd, l) ->
          let v = check v hd in
          List.fold_left (fun v (_, t) -> check v t) v l
      | `Fun { arguments; body } ->
          let v =
            List.fold_left
              (fun v -> function { default = Some d } -> check v d | _ -> v)
              v arguments
          in
          let bound =
            List.fold_left
              (fun v { label; as_variable } ->
                Vars.add (Option.value ~default:label as_variable) v)
              Vars.empty arguments
          in
          let masked = Vars.inter v bound in
          let v = Vars.union v bound in
          let v = check v body in
          Vars.iter
            (fun x ->
              if Vars.mem x v && x <> "_" then (
                let bt = Printexc.get_callstack 0 in
                throw ~bt (Unused_variable (x, Option.get tm.t.Type.pos))))
            bound;
          (* Restore masked variables. The masking variables have been used but
             it does not count for the ones they masked. Bound variables have
             been handled above. *)
          Vars.union masked (Vars.diff v bound)
      | `Let { pat; def; body; _ } ->
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
                  then (
                    let bt = Printexc.get_callstack 0 in
                    throw ~bt (Unused_variable (s, Option.get tm.t.Type.pos))))
              bvpat;
          Vars.union v mask
  in
  (* Unused free variables may remain *)
  ignore (check ~toplevel:true Vars.empty tm)

(* Custom types. *)

module type Custom = sig
  type content

  val t : Type.t
  val to_custom : content -> Custom.t
  val of_custom : Custom.t -> content
  val is_custom : Custom.t -> bool
  val to_term : content -> t
  val of_term : t -> content
end

module type CustomDef = sig
  type content

  val name : string
  val to_string : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
end

module MkCustom (Def : CustomDef) = struct
  module T = Type.Custom.Make (struct
    type content = unit

    let name = Def.name
    let copy_with _ _ = ()
    let occur_check _ _ = ()
    let filter_vars _ l _ = l
    let subtype _ c c' = assert (c = c')

    let sup _ c c' =
      assert (c = c');
      c

    let repr _ _ _ = `Constr (name, [])
    let to_string _ = name
  end)

  let descr = Type.Custom (T.handler ())
  let t = Type.make descr
  let () = Type.register_type Def.name (fun () -> Type.make descr)

  include Custom.Make (struct
    include Def

    let t = t
  end)

  let of_term t =
    match t.term with `Custom c -> of_custom c | _ -> assert false

  let to_term c =
    {
      t = Type.make descr;
      term = `Custom (to_custom c);
      methods = Methods.empty;
      flags = Flags.empty;
    }
end

(** Create a new value. *)
let make ?pos ?t ?flags ?methods e =
  let term = make ?pos ?t ?flags ?methods e in
  let t = match t with Some t -> t | None -> Type.var ?pos () in
  if Lazy.force debug then
    Printf.eprintf "%s (%s): assigned type var %s\n"
      (Pos.Option.to_string t.Type.pos)
      (try to_string term with _ -> "<?>")
      (Repr.string_of_type t);
  term

let rec fresh ~handler { t; term; methods; flags } =
  let term =
    match term with
      | `Cache_env _ | `Int _ | `String _ | `Float _ | `Bool _ | `Custom _ ->
          term
      | `Tuple l -> `Tuple (List.map (fresh ~handler) l)
      | `Null -> `Null
      | `Open (t, t') -> `Open (fresh ~handler t, fresh ~handler t')
      | `Var s -> `Var s
      | `Seq (t, t') -> `Seq (fresh ~handler t, fresh ~handler t')
      | `Let { doc; replace; pat; gen; def; body } ->
          `Let
            {
              doc;
              replace;
              pat;
              gen = List.map (Type.Fresh.make_var handler) gen;
              def = fresh ~handler def;
              body = fresh ~handler body;
            }
      | `List l -> `List (List.map (fresh ~handler) l)
      | `Cast { cast = t; typ } ->
          `Cast { cast = fresh ~handler t; typ = Type.Fresh.make handler typ }
      | `App (t, l) ->
          `App
            ( fresh ~handler t,
              List.map (fun (lbl, t) -> (lbl, fresh ~handler t)) l )
      | `Hide (tm, l) -> `Hide (fresh ~handler tm, l)
      | `Invoke { invoked; invoke_default; meth } ->
          `Invoke
            {
              invoked = fresh ~handler invoked;
              invoke_default = Option.map (fresh ~handler) invoke_default;
              meth;
            }
      | `Encoder encoder ->
          let rec map_encoder (lbl, params) =
            ( lbl,
              List.map
                (function
                  | `Anonymous s -> `Anonymous s
                  | `Encoder enc -> `Encoder (map_encoder enc)
                  | `Labelled (lbl, t) -> `Labelled (lbl, fresh ~handler t))
                params )
          in
          `Encoder (map_encoder encoder)
      | `Fun { free_vars; name; arguments; body } ->
          `Fun
            {
              free_vars;
              name;
              arguments =
                List.map
                  (fun { label; as_variable; default; typ; pos } ->
                    {
                      label;
                      as_variable;
                      default = Option.map (fresh ~handler) default;
                      typ = Type.Fresh.make handler typ;
                      pos;
                    })
                  arguments;
              body = fresh ~handler body;
            }
  in
  {
    t = Type.Fresh.make handler t;
    term;
    methods = Methods.map (fresh ~handler) methods;
    flags;
  }
