(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let debug =
  try
    ignore (Sys.getenv "LIQUIDSOAP_DEBUG_LANG") ;
    true
  with
    | Not_found -> false

(** Error reporting for the parser. *)
exception Unbound of string

module T = Lang_types

(** {1 Values}
  * The way we implement this mini-language is not at all efficient.
  * It should not matter, since very little computation is done here.
  * It is mostly used for a single run on startup to build the sources,
  * and then sometimes for building transitions. Terms are small, no recursion
  * is possible.
  * In order to report informative errors, including runtime errors (invalid
  * values of a valid type given to a FF) we need to keep a complete AST
  * all the way long.
  * We actually don't need the types anymore after the static checking,
  * but I don't want to bother with stripping down to another datatype. *)

type value = { mutable t : T.t ; value : in_value }
and  in_value =
  | Unit
  | Bool    of bool
  | Int     of int
  | String  of string
  | Float   of float
  | Source  of Source.source
  | Request of Request.t option
  | List    of value list
  | Product of value * value
  | Let     of (Doc.item*(string*string)list) * string * value * value
  | Var     of string
  | Seq     of value * value
  | App     of value * (string * value) list
  | Fun     of (string*string*T.t*value option) list * value
               (* [fun ~l1:x1 .. ?li:(xi=defi) .. -> body] =
                * [Fun ((l1,x1,None)..(li,xi,Some defi)..),body] *)
  | FFI     of (string*value option) list *
               (string*value) list *
               ((string*value) list -> in_value)
               (* Second component for params which were already given *)

let fulfilled_ffi = List.for_all (fun (_,opt) -> opt <> None)
let fulfilled = List.for_all (fun (_,_,_,opt) -> opt <> None)

let rec print_value v = match v.value with
  | Unit     -> "()"
  | Bool i   -> string_of_bool i
  | Int i    -> string_of_int i
  | Float f  -> string_of_float f
  | String s -> Printf.sprintf "%S" s
  | Source s -> "<source>"
  | Request s -> "<request>"
  | List l ->
      "[ "^(String.concat "; " (List.map print_value l))^" ]"
  | Product (a,b) ->
      Printf.sprintf "(%s,%s)" (print_value a) (print_value b)
  | Fun ([],v) ->
      (* This is a cheap way to avoid the printing of a body involving
       * let-in or seq constructs. *)
      begin match v.value with
        | Unit | Bool _ | Int _ | Float _ | String _ | Source _ | Request _ ->
            "{"^(print_value v)^"}"
        | _ -> "<fun>"
      end
  | Fun _ | FFI _ -> "<fun>"
  | Var s -> s
  | App (hd,tl) ->
      let tl =
        List.map
          (fun (lbl,v) -> (if lbl="" then "" else lbl^"=")^(print_value v))
          tl
      in
        (print_value hd)^"("^(String.concat "," tl)^")"
  | Let _ | Seq _ ->
      (* print only normal closed forms *)
      assert false

(* {1 Type checking/inference} *)

let (<:) = T.(<:)
let (>:) = T.(>:)

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
  * passed to the function [f] because the latter has no label [lbl].
  * The [first] information tells whether [lbl=x] is the first parameter with
  * label [lbl] in the considered application, which makes the message a bit
  * more helpful. *)
exception No_label of value * string * bool * value

let rec check ?(print_toplevel=false) ~level e =
  (** The toplevel position of the (un-dereferenced) type
    * is the actual parsing position of the value.
    * When we synthesize a type against which the type of the value is unified,
    * we have to set the position information in order not to loose it. *)
  let mk t = T.make ~level ~pos:e.t.T.pos t in
  let mkg t = mk (T.Ground t) in
  match e.value with
  | Var s ->
      (* We instantiate type schematas here.
       * It must be done before any use of the variable's type,
       * otherwise the subtyping will choke on the UVars. *)
      let orig = e.t in
      e.t <- T.instantiate ~level e.t ;
      if debug then
        Printf.eprintf "Instantiate %s : %s into %s\n"
          s (T.print orig) (T.print e.t)
  | Let (_,name,def,body) ->
      check ~level def ;
      let t0 = T.print def.t in
      T.generalize ~level def.t ;
      if debug then
        Printf.eprintf "Generalize %s into %s\n" t0 (T.print def.t) ;
      if print_toplevel then
        Printf.printf "%s \t:: %s\n" name (T.print def.t) ;
      check ~print_toplevel ~level:(level+1) body ;
      e.t >: body.t
  | Unit      -> e.t >: mkg T.Unit
  | Bool    _ -> e.t >: mkg T.Bool
  | Int     _ -> e.t >: mkg T.Int
  | String  _ -> e.t >: mkg T.String
  | Float   _ -> e.t >: mkg T.Float
  | Source  _ -> e.t >: mkg T.Source
  | Request _ -> e.t >: mkg T.Request
  | List l ->
      List.iter (check ~level) l ;
      let pos =
        (* Attach the position of the first item in the list
         * to the type of the list items. It gives more info with type errors
         * when the items in the list are not compatible.
         * WARNING This will become weird with real subtyping in the inference,
         * because the type of elements will only be _a supertype_ of the type
         * of the first item. *)
        match l with e::_ -> e.t.T.pos | [] -> e.t.T.pos
      in
      let v = T.fresh_evar ~level ~pos in
        e.t >: mk (T.List v) ;
        List.iter (fun item -> item.t <: v) l
  | Product (a,b) ->
      check ~level a ; check ~level b ;
      e.t >: mk (T.Product (a.t,b.t))
  | Seq (a,b) ->
      check ~level a ;
      begin match (T.deref a.t).T.descr with
        (* Maybe it's not an output source, but at least it's a source. *)
        | T.Ground T.Unit | T.Ground T.Source -> ()
        | _ ->
            Printf.printf "%s: %s\n%!"
              (T.print_pos (Utils.get_some a.t.T.pos))
              "this value should be a source, or unit."
      end ;
      check ~print_toplevel ~level b ;
      e.t >: b.t
  | App (a,l) ->
      check ~level a ;
      List.iter (fun (_,b) -> check ~level b) l ;
      let p =
        (* If [a] is known to have a function type, get the parameters'
         * optionality from its type. Otherwise, they will all have to be
         * mandatory. *)
        match (T.deref a.t).T.descr with
          | T.Arrow (ap,t) ->
              let rec get_optionality lbl l =
                let rec aux acc = function
                  | [] -> None
                  | (o,lbl',t)::l ->
                      if lbl=lbl' then
                        Some (o, List.rev_append acc l)
                      else
                        aux ((o,lbl',t)::acc) l
                in
                  aux [] l
              in
              let p',_ =
                List.fold_left
                  (fun (p',ap) (lbl,v) ->
                     match get_optionality lbl ap with
                       | None ->
                           let first =
                             not (List.exists (fun (_,l,_) -> l=lbl) p')
                           in
                             raise (No_label (a,lbl,first,v))
                         | Some (o,ap') ->
                             ((o,lbl,v.t)::p'),ap')
                  ([],ap)
                  l
              in
                List.rev p'
          | _ ->
              List.map (fun (lbl,b) -> false,lbl,b.t) l
      in
        begin try
          (* Special case here for intuitive error messages:
           * ignore the flipping of the focus in case error occurs.
           * The inital focus should be on the function's type
           * (not the synthesized one) but then the focus should not follow
           * the function's type but move to the arguments types. *)
          a.t <: T.make ~level ~pos:None (T.Arrow (p,e.t))
        with
          | T.Error (x::T.Flip::tl) -> raise (T.Error (x::tl))
        end
  | Fun (proto,body) ->
      let proto_t =
        List.map
          (function
             | lbl,_,kind,None   -> false,lbl,kind
             | lbl,_,kind,Some v -> check ~level v ;
                                    v.t <: kind ;
                                    true,lbl,v.t)
          proto
      in
        check ~level:(level+(List.length proto)) body ;
        e.t >: mk (T.Arrow (proto_t,body.t))
  | FFI (proto,param,f) ->
      (* As for Var, we instantiate type schemata. *)
      (* TODO take param into account *)
      let orig = e.t in
      e.t <- T.instantiate ~level e.t ;
      if debug then
        Printf.eprintf "Instantiate %s into %s\n"
          (T.print orig) (T.print e.t)

(** {1 Extend the language with builtin values} *)

let builtins : value Plug.plug
  = Plug.create ~doc:"scripting values" "scripting values"

(** Binding stack, not only carrying local bindings but also builtins and
  * script-defined libraries. *)
let bindings : (string * value) list ref = ref []

(* [init_bindings] is called before the parsing of any file,
 * but it should fill the bindings with builtins only the first time.
 * It cannot be called on program instantiation because builtins will not have
 * registered yet into [Lang.builtins]. *)
let init_bindings =
  let init_done = ref false in
    fun () ->
      if not !init_done then begin
        init_done := true ;
        builtins#iter
          (fun name value -> bindings := (name,value)::!bindings)
      end

(** The treatment of topelevel definitions is a bit schizophrenic.
  * The [builtins] plug gets extended with new toplevel definitions as they
  * arise. As a consequence one can get the doc of values created from scripts,
  * but the value associated with the definition doesn't matter much there.
  * The [bindings] stack (used by the parser to lookup variables)
  * adds a seemingly dummy indirection, associating the
  * variable "x" to "x". The point is that this variable "x" has a type which
  * has been computed in the first script, which should be re-used as-is.
  * Replacing "x" by its value [v] at type-checking time would trigger redundant
  * checks. The actual replacement of the stub "x" by [v] will be done during
  * evaluation, by means of the [top_bindings] stack. *)

let top_bindings : (string * value) list ref = ref []

let toplevel_add (doc,params) x v =
  let ptypes =
    match (T.deref v.t).T.descr with
      | T.Arrow (p,_) -> p
      | _ -> []
  in
  let pvalues =
    match v.value with
      | Fun (p,_) -> List.map (fun (l,_,_,o) -> l,o) p
      | FFI (p,_,_) -> p
      | _ -> []
  in
  let params,pvalues =
    List.fold_left
      (fun (params,pvalues) (opt,label,t) ->
         let descr,params =
           try
             List.assoc label params,
             List.remove_assoc label params
           with
             | Not_found -> "",params
         in
         let default,pvalues =
           try
             `Known (List.assoc label pvalues),
             List.remove_assoc label pvalues
           with
             | Not_found -> `Unknown, pvalues
         in
         let item = Doc.trivial (if descr="" then "(no doc)" else descr) in
           item#add_subsection "type" (Doc.trivial (T.print t)) ;
           item#add_subsection "default"
             (Doc.trivial (match default with
                             | `Unknown -> "???"
                             | `Known (Some v) -> print_value v
                             | `Known None -> "None")) ;
           doc#add_subsection
             (if label="" then "(unlabeled)" else label)
             item ;
           params,pvalues)
      (params,pvalues)
      ptypes
  in
    List.iter
      (fun (s,_) ->
         Printf.eprintf "WARNING: Unused @param %S for %s!\n" s x)
      params ;
    doc#add_subsection "type" (Doc.trivial (T.print v.t)) ;
    (* TODO this registration should erase a previous definition of [x].
     * Also, notice that [v] isn't useful here, and could be anything. *)
    builtins#register ~doc x v ;
    bindings := (x,{v with value=Var x})::(!bindings) ;
    top_bindings := (x,v)::(!top_bindings)

(** For internal use. I want to give an ID to sources built by FFI application
  * based on the name under which the FFI is registered.
  * [get_name f] returns the name under which the FFI f is registered. *)
exception F of string
let get_name f =
  try
    builtins#iter
      (fun name v -> match v.value with
         | FFI (_,_,ff) when f == ff -> raise (F name)
         | _ -> ()) ;
    "<ff>"
  with
    | F s -> s

(** {1 Computations} *)

(** This check could be done completely during type analysis,
  * if it were possible to distinguish active sources at this point.. *)
let check_unit_like a = match a.value with
  | Unit -> () | Source s when s#is_output -> ()
  | _ ->
      Printf.printf "%s: %s\n%!"
        (T.print_pos (Utils.get_some a.t.T.pos))
        "this value should be an active source, or unit."

(** [substitute target v a] computes [a[target:=v]].
  * The implementation of eval should only substitute head-normal forms. *)
let rec substitute substitution t =
  let aux = substitute substitution in
  match t.value with
    | Var name ->
        begin try
          let v = List.assoc name substitution in
            (* Don't care about the type (which is the same anyway),
             * keep the initial position (more informative for source naming)
             * so just substitute the value *)
            { t with value = v.value }
        with
          | Not_found -> t
        end
    | Unit | Bool _ | Int _ | String _ | Float _ | Source _ | Request _ -> t
    | Product (a,b) -> { t with value = Product (aux a, aux b) }
    | List l -> { t with value = List (List.map aux l) }
    | App (a,l) -> { t with value =
                       App (aux a, List.map (fun (lbl,b) -> lbl, aux b) l) }
    | Seq (a,b) -> { t with value = Seq (aux a,aux b) }
    | Let (d,l,def,body) ->
        { t with value =
            Let (d, l, aux def,
                 substitute
                   (List.filter (fun (lbl,v) -> lbl<>l) substitution)
                   body) }
    | Fun (proto,body) ->
        let subst,proto =
          List.fold_left
            (fun (subst,proto) (lbl,var,kind,v) ->
               (List.filter (fun (lbl,v) -> lbl<>var) subst),
               (lbl,var,kind,(match v with
                                | None -> v
                                | Some v -> Some (substitute subst v)))::proto)
            (substitution,[])
            proto
        in
          { t with value = Fun (List.rev proto, substitute subst body) }
    | FFI (proto,param,f) ->
        let subst,proto =
          List.fold_left
            (fun (subst,proto) (lbl,v) ->
               (List.filter (fun (l,v) -> lbl<>l) subst),
               (lbl,(match v with
                       | None -> v
                       | Some v -> Some (substitute subst v)))::proto)
            (substitution,[])
            proto
        in
          { t with value = FFI (List.rev proto, param, f) }

(** Helper for performing substitution directly on a FFI prototype. *)
let substitute_proto_ffi subst proto =
  let s =
    substitute subst { t = T.dummy ;
                       value = FFI (proto,[],(fun _ -> assert false)) }
  in
    match s.value with
      | FFI (p,_,_) -> p
      | _ -> assert false

(** [remove_first f l] removes the first element [e] of [l] such that [f e],
  * and returns [e,l'] where [l'] is the list without [e].
  * Asserts that there is such an element. *)
let remove_first filter =
  let rec aux acc = function
    | [] -> assert false
    | hd::tl ->
         if filter hd then
           (hd,List.rev_append acc tl)
         else
           aux (hd::acc) tl
  in aux []

let rec remove_all_first filters l = match filters with
  | [] -> [],l
  | f::fl ->
      let e,l = remove_first f l in
      let le,l = remove_all_first fl l in
        e::le,l

(** Computes the return value of a function after that all of
  * its mandatory arguments have been applied. *)
let rec defaults f = match f.value with
  | Fun ([],body) -> body
  | Fun ((lbl,var,kind,Some v)::tl,body) ->
      (* Do not perform the substitution in a single row,
       * the substitution of the first params must be able to alter the default
       * values for other params. *)
      defaults (substitute [var,eval v] { f with value = Fun (tl,body) })
  | _ -> assert false

(** [apply a l] computes the head-normalized form of the application [a(l)],
  * assuming that [a] and second components of [b] are head-normalized. *)
and apply ?(pos=None) ?(t=T.dummy) a l =
  match a.value with
    | Fun (proto,body) ->
        let targets,proto =
          remove_all_first
            (List.map (fun (lbl,_) (l,_,_,_) -> lbl = l) l)
            proto
        in
        let substitution =
          List.map2
            (fun (_,target,_,_) (lbl,v) -> target,v)
            targets l
        in
          (* We assume that [a] is closed, it is thus safe to
           * substitute [v]s for [target]s on the part of the [proto] where
           * [target] is not bound -- it will have no effect. *)
          begin match
            (substitute substitution { a with value = Fun (proto,body) }).value
          with
            | Fun (proto,body) ->
                let expr =
                  { t = t ;
                    value = Fun (proto,body) }
                in
                  if fulfilled proto then eval (defaults expr) else expr
            | _ -> assert false
          end
    | FFI (proto,params,f) ->
        let _,proto =
          remove_all_first (List.map (fun (l,_) (lbl,_) -> lbl = l) l) proto
        in
        let params = params@l in
        let proto = substitute_proto_ffi l proto in
          {
            t = t ;
            value =
              if fulfilled_ffi proto then
                match
                  f (params@
                     (List.map (function
                                  | lbl,None -> assert false
                                  | lbl,Some e -> lbl,eval e) proto))
                with
                  | Source s as src ->
                      let pos = T.print_single_pos (Utils.get_some t.T.pos) in
                      let id = (get_name f) ^ "@" ^ pos in
                        s#set_id ~definitive:false id ;
                        src
                  | v -> v
              else
                FFI (proto,params,f)
          }
    | _ -> assert false

and eval v = match v.value with
  | Fun _ | FFI _
  | Unit | Bool _ | Int _ | String _ | Float _ | Source _ | Request _ -> v
  | Var x -> assert false
  | List l -> { v with value = List (List.map eval l) }
  | Product (a,b) -> { v with value = Product (eval a, eval b) }
  | App (a,l) -> apply ~t:v.t ~pos:v.t.T.pos (eval a)
                   (List.map (fun (l,b) -> (l,eval b)) l)
  | Seq (a,b) ->
      check_unit_like (eval a) ;
      eval b
  | Let (_,name,def,body) -> eval (substitute [name,eval def] body)

let rec eval_toplevel t = match t.value with
  | Let (comment,name,def,body) ->
      let def = eval def in
        toplevel_add comment name def ;
        eval_toplevel (substitute [name,def] body)
  | Seq (a,b) ->
      check_unit_like (eval_toplevel a) ;
      eval_toplevel b
  | _ -> eval t

let eval_toplevel t = eval_toplevel (substitute !top_bindings t)

(** Simpler definitions for external use. *)
let check e =
  let print_toplevel = !Configure.display_types in
    check ~print_toplevel ~level:(List.length !bindings) e

(* Erase the optional parameters [pos] and [t]. *)
let apply f l = apply f l
