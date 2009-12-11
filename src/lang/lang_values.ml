(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** {1 Kinds}
  *
  * In a sense this could move to Lang_types, but I like to keep that
  * part free of some specificities of liquidsoap, as much as possible. *)

module T = Lang_types

let ref_t ~pos ~level t =
  T.make ~pos ~level
    (T.Constr { T.name = "ref" ; T.params = [T.Invariant,t] })

let zero_t = T.make T.Zero
let succ_t t = T.make (T.Succ t)
let variable_t = T.make T.Variable

let rec type_of_int n = if n=0 then zero_t else succ_t (type_of_int (n-1))

(** A frame kind type is a purely abstract type representing a frame kind.
  * The parameters [audio,video,midi] are intended to be multiplicity types,
  * i.e. types of the form Succ*(Zero|Variable). *)
let frame_kind_t ?pos ?level audio video midi =
  T.make ?pos ?level
    (T.Constr { T.name = "stream_kind" ;
                T.params = [T.Covariant,audio;
                            T.Covariant,video;
                            T.Covariant,midi ] })
let of_frame_kind_t t = match (T.deref t).T.descr with
  | T.Constr { T.name="stream_kind" ; T.params=[_,audio;_,video;_,midi] } ->
      { Frame. audio=audio; video=video; midi=midi }
  | T.EVar (j,c) ->
      let audio = type_of_int (Lazy.force Frame.audio_channels) in
      let video = type_of_int (Lazy.force Frame.video_channels) in
      let midi = type_of_int (Lazy.force Frame.midi_channels) in
        T.bind t (frame_kind_t audio video midi) ;
        { Frame. audio=audio; video=video; midi=midi }
  | _ -> assert false

(** Type of audio formats that can encode frame of a given kind. *)
let format_t ?pos ?level k =
  T.make ?pos ?level
    (T.Constr { T.name = "format" ; T.params = [T.Covariant,k] })

(** Type of sources carrying frames of a given kind. *)
let source_t ?pos ?level k =
  T.make ?pos ?level
    (T.Constr { T.name = "source" ; T.params = [T.Invariant,k] })
let of_source_t t = match (T.deref t).T.descr with
  | T.Constr { T.name = "source" ; T.params = [_,t] } -> t
  | _ -> Printf.eprintf "Not a source_t: %s\n" (T.print (T.deref t)) ;
         assert false

let request_t ?pos ?level k =
  T.make ?pos ?level
    (T.Constr { T.name = "request" ; T.params = [T.Invariant,k] })
let of_request_t t = match (T.deref t).T.descr with
  | T.Constr { T.name = "request" ; T.params = [_,t] } -> t
  | _ -> assert false

let rec type_of_mul ~pos ~level m =
  T.make ~pos ~level
    (match m with
       | Frame.Variable -> assert false
       | Frame.Zero -> T.Zero
       | Frame.Succ m -> T.Succ (type_of_mul ~pos ~level m))

let type_of_format ~pos ~level f =
  let kind = Encoder.kind_of_format f in
  let audio = type_of_mul ~pos ~level kind.Frame.audio in
  let video = type_of_mul ~pos ~level kind.Frame.video in
  let midi  = type_of_mul ~pos ~level kind.Frame.midi in
    format_t ~pos ~level (frame_kind_t ~pos ~level audio video midi)

(** {1 Terms}
  * The way we implement this mini-language is not very efficient.
  * It should not matter, since very little computation is done here.
  * It is mostly used for a single run on startup to build the sources,
  * and then sometimes for building transitions. Terms are small, no recursion
  * is possible.
  * In order to report informative errors, including runtime errors (invalid
  * values of a valid type given to a FF) we need to keep a complete AST
  * all the way long.
  * We actually don't need the types anymore after the static checking,
  * but I don't want to bother with stripping down to another datatype. *)

type term = { mutable t : T.t ; term : in_term }
and let_t = {
  doc : Doc.item * (string*string) list ;
  var : string ;
  gen : (int*T.constraints) list ref ;
  def : term ;
  body : term
}
and  in_term =
  | Unit
  | Bool    of bool
  | Int     of int
  | String  of string
  | Float   of float
  | Encoder of Encoder.format
  | List    of term list
  | Product of term * term
  | Ref     of term
  | Let     of let_t
  | Var     of string
  | Seq     of term * term
  | App     of term * (string * term) list
  | Fun     of (string*string*T.t*term option) list * term
               (* [fun ~l1:x1 .. ?li:(xi=defi) .. -> body] =
                * [Fun ((l1,x1,None)..(li,xi,Some defi)..),body] *)

(* Only used for printing very simple functions. *)
let rec is_ground x = match x.term with
  | Unit | Bool _ | Int _ | Float _ | String _
  | Encoder _ -> true
  | Ref x -> is_ground x
  | _ -> false

(** Print terms, (almost) assuming they are in normal form. *)
let rec print_term v = match v.term with
  | Unit     -> "()"
  | Bool i   -> string_of_bool i
  | Int i    -> string_of_int i
  | Float f  -> string_of_float f
  | String s -> Printf.sprintf "%S" s
  | Encoder e -> Encoder.string_of_format e
  | List l ->
      "["^(String.concat ", " (List.map print_term l))^"]"
  | Product (a,b) ->
      Printf.sprintf "(%s,%s)" (print_term a) (print_term b)
  | Ref a ->
      Printf.sprintf "ref (%s)" (print_term a)
  | Fun ([],v) when is_ground v -> "{"^(print_term v)^"}"
  | Fun _ -> "<fun>"
  | Var s -> s
  | App (hd,tl) ->
      let tl =
        List.map
          (fun (lbl,v) -> (if lbl="" then "" else lbl^"=")^(print_term v))
          tl
      in
        (print_term hd)^"("^(String.concat "," tl)^")"
  | Let _ | Seq _ -> assert false

(** Maps a function on all types occurring in a term.
  * Ignores variable generalizations. *)
let rec map_types f tm = match tm.term with
  | Unit | Bool _ | Int _ | String _ | Float _ | Encoder _ | Var _ ->
      { tm with t = f tm.t }
  | Ref r ->
      { t = f tm.t ;
        term = Ref (map_types f r) }
  | Product (a,b) ->
      { t = f tm.t ;
        term = Product (map_types f a, map_types f b) }
  | Seq (a,b) ->
      { t = f tm.t ;
        term = Seq (map_types f a, map_types f b) }
  | List l ->
      { t = f tm.t ;
        term = List (List.map (map_types f) l) }
  | App (hd,l) ->
      { t = f tm.t ;
        term = App (map_types f hd,
                    List.map (fun (lbl,v) -> lbl, map_types f v) l) }
  | Fun (p,v) ->
      let aux = function
        | (lbl,var,t,None) -> lbl, var, f t, None
        | (lbl,var,t,Some tm) -> lbl, var, f t, Some (map_types f tm)
      in
        { t = f tm.t ;
          term = Fun (List.map aux p, map_types f v) }
  | Let l ->
      { t = f tm.t ;
        term = Let { l with def = map_types f l.def ;
                            body = map_types f l.body } }

(** Folds [f] over almost all types occurring in a term,
  * skipping as much as possible while still
  * guaranteeing that [f] will see all variables. *)
let rec fold_types f gen x tm = match tm.term with
  | Unit | Bool _ | Int _ | String _ | Float _ | Encoder _ | Var _ ->
      f gen x tm.t
  | List l ->
      List.fold_left (fun x tm -> fold_types f gen x tm) (f gen x tm.t) l
  (* In the next three cases, don't care about tm.t, nothing "new" in it. *)
  | Ref r ->
      fold_types f gen x r
  | Product (a,b) | Seq (a,b) ->
      fold_types f gen (fold_types f gen x a) b
  | App (tm,l) ->
      let x = fold_types f gen x tm in
        List.fold_left (fun x (_,tm) -> fold_types f gen x tm) x l
  | Fun (p,v) ->
      fold_types f gen
        (List.fold_left
           (fun x -> function
              | (_,_,t,Some tm) ->
                  fold_types f gen (f gen x t) tm
              | (_,_,t,None) ->
                  f gen x t)
           x
           p)
        v
  | Let {gen={contents=gen'};def=def;body=body} ->
      let x = fold_types f (gen'@gen) x def in
        fold_types f gen x body

(** Values are normal forms of terms. *)
module V =
struct
  type value = { mutable t : T.t ; value : in_value }
  and full_env = (string * ((int*T.constraints)list*value)) list
  and in_value =
    | Unit
    | Bool    of bool
    | Int     of int
    | String  of string
    | Float   of float
    | Source  of Source.source
    | Request of Request.raw Request.t option (* cf. terms *)
    | Encoder of Encoder.format
    | List    of value list
    | Product of value * value
    | Ref     of value ref
    (** In the next two constructors the first environment contains the
      * parameters already passed to the closure. It cannot be integrated in
      * the second one immediately as further applications will insert new
      * bindings between the first and second environments (cf. apply). *)
    | Fun     of (string * string * value option) list *
                 full_env * full_env * term
    | FFI     of (string * string * value option) list *
                 full_env * full_env * (full_env -> T.t -> value)

  type env = (string*value) list

  let rec print_value v = match v.value with
    | Unit     -> "()"
    | Bool i   -> string_of_bool i
    | Int i    -> string_of_int i
    | Float f  -> string_of_float f
    | String s -> Printf.sprintf "%S" s
    | Source s -> "<source>"
    | Request s -> "<request>"
    | Encoder e -> Encoder.string_of_format e
    | List l ->
        "["^(String.concat ", " (List.map print_value l))^"]"
    | Ref a ->
        Printf.sprintf "ref (%s)" (print_value !a)
    | Product (a,b) ->
        Printf.sprintf "(%s,%s)" (print_value a) (print_value b)
    | Fun (_,_,_,x) when is_ground x -> "{"^print_term x^"}"
    | Fun _ | FFI _ -> "<fun>"

  let map_env f env = List.map (fun (s,(g,v)) -> s, (g, f v)) env

  let tm_map_types = map_types

  (** Map a function on all types occurring in a value. *)
  let rec map_types f v = match v.value with
    | Unit | Bool _ | Int _ | String _ | Float _ | Encoder _ ->
        { v with t = v.t }
    | Product (a,b) ->
        { t = f v.t ;
          value = Product (map_types f a, map_types f b) }
    | List l ->
        { t = f v.t ;
          value = List (List.map (map_types f) l) }
    | Fun (p,applied,env,tm) ->
        let aux = function
          | lbl, var, None -> lbl, var, None
          | lbl, var, Some v -> lbl, var, Some (map_types f v)
        in
          { t = f v.t ;
            value = 
              Fun (List.map aux p,
                   map_env (map_types f) applied,
                   map_env (map_types f) env,
                   tm_map_types f tm) }
    | FFI (p,applied,env,ffi) ->
        let aux = function
          | lbl, var, None -> lbl, var, None
          | lbl, var, Some v -> lbl, var, Some (map_types f v)
        in
          { t = f v.t ;
            value = FFI (List.map aux p,
                         map_env (map_types f) applied,
                         map_env (map_types f) env,
                         ffi) }
    (* In the case on instantiate (currently the only use of map_types)
     * no type instantiation should occur in the following cases (f should
     * be the identity): one cannot change the type of such objects once
     * they are created. *)
    | Source s ->
        assert (f v.t = v.t) ;
        v
    | Request s ->
        assert (f v.t = v.t) ;
        v
    | Ref r ->
        assert (f v.t = v.t) ;
        r := map_types f !r ;
        v

end

(** {1 Built-in values and toplevel definitions} *)

let builtins : (((int*T.constraints) list) * V.value) Plug.plug
  = Plug.create ~duplicates:false ~doc:"scripting values" "scripting values"

(* {1 Type checking/inference} *)

let (<:) = T.(<:)
let (>:) = T.(>:)

let rec value_restriction t = match t.term with
  | Fun _ -> true
  | List l -> List.for_all value_restriction l
  | Product (a,b) -> value_restriction a && value_restriction b
  | _ -> false

exception Unbound of T.pos option * string

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
  * passed to the function [f] because the latter has no label [lbl].
  * The [first] information tells whether [lbl=x] is the first parameter with
  * label [lbl] in the considered application, which makes the message a bit
  * more helpful. *)
exception No_label of term * string * bool * term

(* Type-check an expression.
 * [level] should be the sum of the lengths of [env] and [builtins],
 * that is the size of the typing context, that is the number of surrounding
 * abstractions. *)
let rec check ?(print_toplevel=false) ~level ~env e =
  (** The role of this function is not only to type-check but also to assign
    * meaningful levels to type variables, and unify the types of all occurences
    * of the same variable, since the parser does not do that (anymore). *)
  assert (e.t.T.level = -1) ;
  e.t.T.level <- level ;
  (** The toplevel position of the (un-dereferenced) type
    * is the actual parsing position of the value.
    * When we synthesize a type against which the type of the term is unified,
    * we have to set the position information in order not to loose it. *)
  let pos = e.t.T.pos in
  let mk t = T.make ~level ~pos t in
  let mkg t = mk (T.Ground t) in
  match e.term with
  | Unit      -> e.t >: mkg T.Unit
  | Bool    _ -> e.t >: mkg T.Bool
  | Int     _ -> e.t >: mkg T.Int
  | String  _ -> e.t >: mkg T.String
  | Float   _ -> e.t >: mkg T.Float
  | Encoder f -> e.t >: type_of_format ~pos:e.t.T.pos ~level f
  | List l ->
      List.iter (check ~level ~env) l ;
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
      check ~level ~env a ; check ~level ~env b ;
      e.t >: mk (T.Product (a.t,b.t))
  | Ref a ->
      check ~level ~env a ;
      e.t >: ref_t ~pos ~level a.t
  | Seq (a,b) ->
      check ~env ~level a ;
      begin match (T.deref a.t).T.descr with
        (* Maybe it's not an active source, but at least it's a source. *)
        | T.Ground T.Unit | T.Constr {T.name="source"} -> ()
        | _ ->
            Printf.printf "%s: %s\n%!"
              (T.print_pos (Utils.get_some a.t.T.pos))
              "this value should be a source, or unit."
      end ;
      check ~print_toplevel ~level ~env b ;
      e.t >: b.t
  | App (a,l) ->
      check ~level ~env a ;
      List.iter (fun (_,b) -> check ~env ~level b) l ;
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
      let base_check = check ~level ~env in
      let proto_t,env,level =
        List.fold_left
          (fun (p,env,level) -> function
             | lbl,var,kind,None   ->
                 if debug then
                   Printf.eprintf "Assigning level %d to %s (%s).\n"
                     level var (T.print kind) ;
                 kind.T.level <- level ;
                 (false,lbl,kind)::p, (var,([],kind))::env, level+1
             | lbl,var,kind,Some v ->
                 if debug then
                   Printf.eprintf "Assigning level %d to %s (%s).\n"
                     level var (T.print kind) ;
                 kind.T.level <- level ;
                 base_check v ;
                 v.t <: kind ;
                 (true,lbl,kind)::p, (var,([],kind))::env, level+1)
          ([],env,level)
          proto
      in
      let proto_t = List.rev proto_t in
        check ~level ~env body ;
        e.t >: mk (T.Arrow (proto_t,body.t))
  | Var var ->
      let generalized,orig =
        try
          List.assoc var env
        with
          | Not_found ->
              begin match builtins#get var with
                | Some (g,v) -> g,v.V.t
                | None -> raise (Unbound (e.t.T.pos,var))
              end
      in
        e.t >: T.instantiate ~level ~generalized orig ;
        if debug then
          Printf.eprintf "Instantiate %s[%d] : %s becomes %s\n"
            var (T.deref e.t).T.level (T.print orig) (T.print e.t)
  | Let {gen=gen; var=name; def=def; body=body} ->
      check ~level:level ~env def ;
      let generalized =
        if value_restriction def then
          let f gen x t =
            let x' =
              T.filter_vars
                (function
                   | { T. descr = T.EVar (i,c) ; level = l } ->
                       not (List.mem_assoc i x) &&
                       not (List.mem_assoc i gen) && l >= level
                   | _ -> assert false)
                t
            in
              x'@x
          in
            fold_types f [] [] def
        else
          []
      in
      let env = (name,(generalized,def.t))::env in
        gen := generalized ;
        if print_toplevel then
          Printf.printf "%s \t: %s\n%!" name (T.print ~generalized def.t) ;
        check ~print_toplevel ~level:(level+1) ~env body ;
        e.t >: body.t

(** For internal use. I want to give an ID to sources built by FFI application
  * based on the name under which the FFI is registered.
  * [get_name f] returns the name under which the FFI f is registered. *)
exception F of string
let get_name f =
  try
    builtins#iter
      (fun name (_,v) -> match v.V.value with
         | V.FFI (_,_,_,ff) when f == ff -> raise (F name)
         | _ -> ()) ;
    "<ff>"
  with
    | F s -> s

(** {1 Computations} *)

(** This check could be done completely during type analysis,
  * if it were possible to distinguish active sources at this point.. *)
let check_unit_like a = match a.V.value with
  | V.Unit -> () | V.Source s when s#is_output -> ()
  | _ ->
      Printf.printf "%s: %s\n%!"
        (T.print_pos (Utils.get_some a.V.t.T.pos))
        "this value should be an active source, or unit."

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

(** Evaluation has to be typed, because some FFI's have a behavior
  * that depends on their return type (for example, source and request
  * creations). This is annoying but we really need a type inference kind
  * of mechanism to obtain the information that such functions need,
  * so this seems like the right thing to do.
  *
  * So, when we evaluate a variable of type T, we have to lookup
  * a let-definition, whose type has T as an instance, instantiate it
  * and unify it with T. But this is not enough: we need the types
  * inside the definition to get instantiated properly too.
  * This also requires to duplicate the definition before
  * instantiation, to avoid sharing the instantiation with
  * the definition itself and its future/past instantiations. *)

let instantiate ~generalized def =
  (* Levels don't matter since we're never going to generalize. *)
  if generalized=[] then def else
    V.map_types (T.instantiate ~generalized ~level:0) def

let lookup env var ty =
  let generalized,def = List.assoc var env in
  let def = instantiate ~generalized def in
    def.V.t <: ty ;
    def

let rec eval ~env tm =
  let mk v = { V.t = tm.t ; V.value = v } in
    match tm.term with
      | Unit -> mk (V.Unit)
      | Bool    x -> mk (V.Bool x)
      | Int     x -> mk (V.Int x)
      | String  x -> mk (V.String x)
      | Float   x -> mk (V.Float x)
      | Encoder x -> mk (V.Encoder x)
      | List l -> mk (V.List (List.map (eval ~env) l))
      | Product (a,b) -> mk (V.Product (eval ~env a, eval ~env b))
      | Ref v -> mk (V.Ref (ref (eval ~env v)))
      | Let {gen={contents=generalized};var=x;def=v;body=b} ->
          (* It should be the case that generalizable variables don't
           * get instantiated in any way when evaluating the definition.
           * But we don't double-check it. *)
          let v = eval ~env v in
          eval ~env:((x,(generalized,v))::env) b
      | Fun (p,body) ->
          let p =
            List.map
              (function
                 | (lbl,var,_,Some v) -> lbl,var,Some (eval ~env v)
                 | (lbl,var,_,None) -> lbl,var,None)
              p
          in
            mk (V.Fun (p,[],env,body))
      | Var var ->
          lookup env var tm.t
      | Seq (a,b) ->
          check_unit_like (eval ~env a) ;
          eval ~env b
      | App (f,l) ->
          apply ~t:tm.t (eval ~env f) (List.map (fun (l,t) -> l, eval ~env t) l)

and apply ?(t=T.dummy) f l =
  let mk v = { V.t = t ; V.value = v } in
  (* Extract the components of the function, whether it's explicit
   * or foreign, together with a rewrapping function for creating
   * a closure in case of partial application. *)
  let p,pe,e,f,rewrap =
    match f.V.value with
      | V.Fun (p,pe,e,body) ->
          p,pe,e,
          (fun env _ -> eval ~env body),
          (fun p pe e -> mk (V.Fun (p,pe,e,body)))
      | V.FFI (p,pe,e,f) ->
          p,pe,e,
          f,
          (fun p pe e -> mk (V.FFI (p,pe,e,f)))
      | _ -> assert false
  in
  let pe,p =
    List.fold_left
      (fun (pe,p) (lbl,v) ->
         let (_,var,_),p =
           remove_first (fun (l,_,_) -> l=lbl) p
         in
           (var,([],v))::pe, p)
      (pe,p) l
  in
    if List.exists (fun (_,_,x) -> x=None) p then
      (* Partial application. *)
      rewrap p pe e
    else
      (* XXX Contrary to older implementation of eval,
       * we do not assign location-based IDs to sources
       * (e.g. add@L13C4). *)
      let pe =
        List.fold_left
          (fun pe (_,var,v) ->
             (var,
              (* Set the position information on FFI's default values.
               * Cf. r5008: if an Invalid_value is raised on a default value,
               * which happens with the mount/name params of output.icecast.*,
               * the printing of the error should succeed at getting a position
               * information. *)
              let v = Utils.get_some v in
                [],
                { v with V.t = T.make ~pos:t.T.pos (T.Link v.V.t) })::pe)
          pe p
      in
      let v = f (List.rev_append pe e) t in
        (* Similarly here, the result of an FFI call should have some position
         * information. For example, if we build a fallible source and pass
         * it to an operator that expects an infallible one, an error
         * is issued about that FFI-made value and a position is needed. *)
        { v with V.t = T.make ~pos:t.T.pos (T.Link v.V.t) }

(** Add toplevel definitions to [builtins] so they can be looked
  * during the evaluation of the next scripts.
  * Also try to generate a structured documentation from the source code. *)
let toplevel_add (doc,params) x ~generalized v =
  let ptypes =
    match (T.deref v.V.t).T.descr with
      | T.Arrow (p,_) -> p
      | _ -> []
  in
  let pvalues =
    match v.V.value with
      | V.Fun (p,_,_,_) -> List.map (fun (l,_,o) -> l,o) p
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
           item#add_subsection "type" (Doc.trivial (T.print ~generalized t)) ;
           item#add_subsection "default"
             (Doc.trivial (match default with
                             | `Unknown -> "???"
                             | `Known (Some v) -> V.print_value v
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
    doc#add_subsection "type" (Doc.trivial (T.print ~generalized v.V.t)) ;
    builtins#register ~doc x (generalized,v)

let rec eval_toplevel t =
  match t.term with
    | Let {doc=comment;
           gen={contents=generalized};
           var=name;def=def;body=body} ->
        let def = eval ~env:builtins#get_all def in
          toplevel_add comment name ~generalized def ;
          eval_toplevel body
    | Seq (a,b) ->
        check_unit_like
          (let v = eval_toplevel a in
             if v.V.t.T.pos = None then { v with V.t = a.t } else v) ;
        eval_toplevel b
    | _ -> eval ~env:builtins#get_all t

(** Simpler definitions for external use. *)

let check e =
  let print_toplevel = !Configure.display_types in
    check ~print_toplevel ~level:(List.length builtins#get_all) ~env:[] e
