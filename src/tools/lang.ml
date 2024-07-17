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

(* The scripting language needs to have functions as first-class values,
 * at least for providing a nice way to program transitions.
 * We also want labels and optional parameters for comfort.
 * And we want static inferred types, because it's GOOD. *)

let debug =
  try
    ignore (Sys.getenv "LIQUIDSOAP_DEBUG_LANG") ;
    true
  with
    | Not_found -> false

(* {1 Source} *)

type kind = in_kind ref
and  in_kind =
  | Unit_t
  | Bool_t
  | Int_t
  | String_t
  | Float_t
  | Source_t
  | Request_t
  | List_t    of kind
  | Product_t of kind * kind
  | Fun_t     of (bool*string*kind) list * kind
  | Unknown   of int
  | Ref       of kind

let dummy_kind = ref (Unknown (-1))

let rec print_kind ?(par=false) k = match !k with
  | Unit_t -> "unit"
  | String_t -> "string"
  | Bool_t -> "bool"
  | Int_t -> "int"
  | Float_t -> "float"
  | Source_t -> "source"
  | Request_t -> "request"
  | Product_t (a,b) ->
      let par = true in
        Printf.sprintf "(%s*%s)" (print_kind ~par a) (print_kind ~par b)
  | List_t t -> Printf.sprintf "[%s]" (print_kind ~par:false t)
  | Unknown i -> Printf.sprintf "'%d" i
  | Ref x -> assert (x!=k) ; print_kind x
  | Fun_t (p,t) ->
      (if par then "((" else "(") ^
      (String.concat ", "
         (List.map
            (fun (opt,lbl,kind) ->
               (if lbl <> "" then
                  (if opt then "?" else "~") ^ lbl ^ ":"
                else "") ^ (print_kind ~par:true kind))
            p)) ^ ") -> " ^
      (print_kind ~par:false t) ^
      (if par then ")" else "")

let rec fulfilled_kind k = match !k with
  | Fun_t (p,t) when List.for_all (fun (b,_,_) -> b) p -> true
  | Ref t -> fulfilled_kind t
  | _ -> false

let rec defaults_kind k = match !k with
  | Fun_t (p,t) -> t
  | Ref t -> defaults_kind t
  | _ -> assert false

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

type pos = Lexing.position*Lexing.position

let dummy_pos = Lexing.dummy_pos,Lexing.dummy_pos

let print_single_pos l =
  let file = if l.Lexing.pos_fname="" then "" else l.Lexing.pos_fname^"/" in
  let line,col = l.Lexing.pos_lnum, (l.Lexing.pos_cnum-l.Lexing.pos_bol) in
    Printf.sprintf "%sL%dC%d" file line (col+1)

let print_pos (start,stop) =
  let f l = l.Lexing.pos_lnum, (l.Lexing.pos_cnum-l.Lexing.pos_bol) in
  let lstart,cstart = f start in
  let lstop,cstop = f stop in
    if lstart = lstop then
      Printf.sprintf "At line %d, char %d-%d" lstart cstart cstop
    else
      Printf.sprintf "At line %d char %d - line %d char %d"
        lstart cstart lstop cstop

type value = { pos : pos ; kind : kind ; value : in_value }
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
  | Let     of string * value * value
  | Var     of string
  | Seq     of value * value
  | App     of value * (string * value) list
  | Fun     of (string*string*kind*value option) list * value
               (* [fun ~l1:x1 .. ?li:(xi=defi) .. -> body] =
                * Fun ((l1,x1,None)..(li,xi,Some defi)..),body *)
  | FFI     of (string*value option) list *
               (string*value) list *
               ((string*value) list -> in_value)
               (* Second component for params which were already given *)

let dummy_value =
  { pos = dummy_pos ; kind = dummy_kind ; value = Unit }

let around a b = fst a.pos, snd b.pos

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
  | Fun ([],v) -> "{"^(print_value v)^"}"
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

let rec iter_sources f v = match v.value with
  | Source s -> f s
  | Unit | Bool _ | Int _ | Float _ | String _ | Request _ -> ()
  | List l -> List.iter (iter_sources f) l
  | Let (_,a,b) | Product (a,b) | Seq (a,b) ->
      iter_sources f a ; iter_sources f b
  | Var _ -> ()
  | App (a,l) ->
      iter_sources f a ;
      List.iter (fun (_,v) -> iter_sources f v) l
  | Fun (proto,body) ->
      iter_sources f body ;
      List.iter (fun (_,_,_,v) -> match v with
                   | Some v -> iter_sources f v
                   | None -> ()) proto
  | FFI (proto,acc,body) ->
      List.iter (fun (_,v) -> match v with
                   | Some v -> iter_sources f v
                   | None -> ()) proto ;
      List.iter (fun (_,v) -> iter_sources f v) acc

(** {1 Extend the language with builtin values} *)

let builtins : value Plug.plug
  = Plug.create ~doc:"liqScript's builtin values" "liqScript builtins"

(** Binding stack, not only carrying local bindings but also libraries. *)
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

(** Don't add toplevel definitions directly to [bindings].
  * Put a variable instead, so that type inference will not be done twice.
  * Open variables will be looked up in the toplevel stack at evaluation. *)
let top_bindings : (string * value) list ref = ref []
let toplevel_add x v =
  bindings := (x,{v with value=Var x})::(!bindings) ;
  top_bindings := (x,v)::(!top_bindings)

let toplevel_lookup var =
  try
    List.assoc var !top_bindings
  with
    | Not_found -> assert false

(** More specific stuff for defining builtin operators and sources, as
  * foreign functions returning a Source.
  * There is a slight language abuse.. hope it doesn't bother the reader. *)

type parameters = (string*value) list
type operator_proto = (string*kind*value option*string option) list

let doc_of_prototype_item t d doc =
  let doc = match doc with None -> "(no doc)" | Some d -> d in
  let item = new Doc.item doc in
    item#add_subsection "type" (Doc.trivial (print_kind t)) ;
    item#add_subsection "default"
      (match d with
	 | None -> Doc.trivial "None"
	 | Some d -> Doc.trivial (print_value d)) ;
    item

(* TODO: author, etc. *)
type doc_flag = Hidden | Deprecated | Experimental
let string_of_flag = function
  | Hidden -> "hidden"
  | Deprecated -> "deprecated"
  | Experimental -> "experimental"

let to_doc category flags main_doc proto =
  let item = new Doc.item main_doc in
  let kind = Fun_t (List.map (fun (l,t,d,doc) -> d<>None,l,t) proto,
                    ref Source_t) in
    item#add_subsection "category" (Doc.trivial category) ;
    item#add_subsection "type" (Doc.trivial (print_kind (ref kind))) ;
    List.iter
      (fun f -> item#add_subsection "flag" (Doc.trivial (string_of_flag f)))
      flags;
    List.iter
      (fun (l,t,d,doc) ->
         item#add_subsection
           (if l = "" then "(unlabeled)" else l)
           (doc_of_prototype_item t d doc)) (List.rev proto) ;
    item

let operator_kind p =
  ref (Fun_t (List.map (fun (lbl,kind,opt,_) -> (opt<>None,lbl,kind)) p,
              ref Source_t))

let operator_value p f =
  { pos = dummy_pos ;
    kind = operator_kind p ;
    value = FFI (List.map (fun (lbl,_,opt,_) -> lbl,opt) p,
                 [],fun x ->
                      let src : Source.source = f x in
                      let id =
                        match (List.assoc "id" x).value with
                          | String s -> s
                          | _ -> assert false
                      in
                        if id <> "" then src#set_id id ;
                        Source src) }

type category =
  | Input | Output | TrackProcessing | SoundProcessing | Visualization
let string_of_category = function
  | Input -> "Input"
  | Output -> "Output"
  | TrackProcessing -> "Track Processing"
  | SoundProcessing -> "Sound Processing"
  | Visualization -> "Visualization"
let string_of_category x = "Source / " ^ string_of_category x

let add_operator ~category ~descr ?(flags=[]) name proto f =
  let proto =
    ("id",ref String_t,
     Some { dummy_value with value = String "" },
     Some "Force the value of the source ID")::proto
  in
  let value = operator_value proto f in
  let category = string_of_category category in
    builtins#register ~doc:(to_doc category flags descr proto) name value

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

let check_unit_like a = match a.value with
  | Unit -> () | Source s when s#is_output -> ()
  | _ ->
      Printf.printf "%s: %s\n%!"
        (print_pos a.pos)
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
    | Let (l,def,body) ->
        { t with value =
            Let (l, aux def,
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
    substitute subst { kind = dummy_kind ; pos = dummy_pos ;
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
and apply ?(pos=dummy_pos) ?(kind=dummy_kind) a l =
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
                  { kind = kind ;
                    pos = pos ;
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
            kind = kind ;
            pos = pos ;
            value =
              if fulfilled_ffi proto then
                match
                  f (params@
                     (List.map (function
                                  | lbl,None -> assert false
                                  | lbl,Some e -> lbl,eval e) proto))
                with
                  | Source s as src ->
                      let pos = print_single_pos (fst a.pos) in
                      let id = (get_name f) ^ "@" ^ pos in
                        s#set_id ~definitive:false id ;
                        src
                  | v -> v
              else
                FFI (proto,params,f)
          }
    | _ -> assert false

and eval t = match t.value with
  | Fun _ | FFI _
  | Unit | Bool _ | Int _ | String _ | Float _ | Source _ | Request _ -> t
  | Var x -> toplevel_lookup x
  | List l -> { t with value = List (List.map eval l) }
  | Product (a,b) -> { t with value = Product (eval a, eval b) }
  | App (a,l) -> apply ~kind:t.kind ~pos:t.pos (eval a)
                   (List.map (fun (l,b) -> (l,eval b)) l)
  | Seq (a,b) ->
      check_unit_like (eval a) ;
      eval b
  | Let (name,def,body) -> eval (substitute [name,eval def] body)

let rec eval_toplevel t = match t.value with
  | Let (name,def,body) ->
      let def = eval def in
        toplevel_add name def ;
        eval_toplevel (substitute [name,def] body)
  | Seq (a,b) ->
      check_unit_like (eval_toplevel a) ;
      eval_toplevel b
  | _ -> eval t

(* Type checking/inference *)

let fresh_kindvar =
  let c = ref 0 in
    fun () -> incr c ; ref (Unknown !c)

let rec deref t = match !t with
  | Ref x -> deref x
  | _ -> t

let bind a b =
  let a = deref a in
  let b = deref b in
    if b != a then a := Ref b

let deref x = !(deref x)

(* This expression at POS has type X but is used with type Y *)
exception Unification_failed of pos * kind * kind
exception In_unification_failed
exception Wrong_label of pos * string
exception In_wrong_label of string

let rec lbl_mem lbl = function
    [] -> false
  | (_,lb,_)::q -> if lb=lbl then true else lbl_mem lbl q

exception No_common_lbl
let rec find_lbl l1 l2 = match (l1,l2) with
  | [],l -> raise No_common_lbl
  | (opt,lbl,pt)::p , pp  -> if lbl_mem lbl pp then lbl else find_lbl p pp

let rec lbl_remove lbl = function
    [] -> []
  | (_,lb,_ as t)::q -> if lb=lbl then q else t::(lbl_remove lbl q)

let rec lbl_type lbl = function
    [] -> raise (In_wrong_label lbl)
  | (_,lb,typ)::q -> if lb=lbl then typ else (lbl_type lbl q)

let unify a b =
  let rec aux a b =
    if debug then Printf.eprintf "%s ?= %s\n%!" (print_kind a) (print_kind b) ;
    match deref a, deref b with
      | Unknown _, _ -> bind a b
      | _, Unknown _ -> bind b a
      | List_t t1, List_t t2 -> aux t1 t2
      | Product_t (a,b), Product_t (aa,bb) -> aux a aa ; aux b bb
	  (* Mmmm I don't feel good here. Let's be safe, do not try to
	   * do crazy guesses, we only need simple cases.
	   * After all, OCaml has some limitations here too. *)
      | Fun_t (p,t), Fun_t (pp,tt) ->
          let fun_t p t = if p = [] then t else ref (Fun_t (p,t)) in
            begin try
              let lbl = find_lbl p pp in
                aux (lbl_type lbl p) (lbl_type lbl pp);
                aux (fun_t (lbl_remove lbl p) t) (fun_t (lbl_remove lbl pp) tt)
            with No_common_lbl ->
              let fresh = fresh_kindvar () in
                aux t (fun_t pp fresh);
                aux tt (fun_t p fresh)
            end
      | a,b ->
         (* The remaining cases are the base types, thanks to deref. *)
         if a <> b then raise In_unification_failed
  (* TODO It would be good to raise errors about mismatch on subtypes
   * rather than errors about the full initial attempt. For exemple get
   * "is string but shout be int" instead of having "is int->.. instead of
   * string->..". The problem is that we don't have any precise position
   * for subtypes. *)
  in
    try aux a.kind b with
      | In_unification_failed -> raise (Unification_failed (a.pos,a.kind,b))

(** Helper function for type error warnings.
  * [get_mandatory_param v] returns:
  * if [v] is a function: [Funsome p] if it has a mandatory param [p], [FunNone]
  * if it has no mandatory param. [NonFun] if [v] isn't a function. *)
type gmp = FunSome of (string*kind) | FunNone | NonFun
let rec get_mandatory_param k = match !k with
  | Fun_t (p,t) ->
      let p = List.filter (fun (b,l,k) -> not b) p in
        if p = [] then match get_mandatory_param t with
	  | NonFun -> FunNone
	  | FunNone -> FunNone
	  | FunSome p -> FunSome p
	else FunSome (let (_,l,k) = List.hd p in l,k)
  | Ref t -> get_mandatory_param t
  | _ -> NonFun

let rec check tree = match tree.value with
  | Unit      -> unify tree (ref Unit_t)
  | Bool    _ -> unify tree (ref Bool_t)
  | Int     _ -> unify tree (ref Int_t)
  | String  _ -> unify tree (ref String_t)
  | Float   _ -> unify tree (ref Float_t)
  | Source  _ -> unify tree (ref Source_t)
  | Request _ -> unify tree (ref Request_t)
  | List l ->
      let v = fresh_kindvar () in
        unify tree (ref (List_t v)) ;
        List.iter check l ;
        List.iter (fun e -> unify e v) l
  | Product (a,b) ->
      unify tree (ref (Product_t (a.kind,b.kind))) ;
      check a ; check b
  | Var s -> ()
  | Seq (a,b) ->
      check a ;
      if deref a.kind <> Unit_t && deref a.kind <> Source_t then begin
        match get_mandatory_param a.kind with
          | FunNone ->
              Printf.printf "%s: this value should have type unit or source, but is a function with only optional parameters remaining: did you forget to apply it to the empty parameter sequence ?\n%!" (print_pos a.pos)
          | FunSome (lbl,kind) ->
              Printf.printf "%s: this value should have type unit or source, but it is a function: did you forget to apply the %s parameter ?\n%!" (print_pos a.pos) (if lbl="" then print_kind kind else lbl^":"^print_kind kind)
          | NonFun -> Printf.printf "%s: this value has type %s but should have type unit or source!\n%!" (print_pos a.pos) (print_kind a.kind)
      end ;
      check b
  | App (a,l) ->
      List.iter (fun (_,b) -> check b) l ; check a ;
	  begin match (deref a.kind) with
        | Fun_t (p,t) ->
            let rec check_args args kinds = match args with
              | []->()
              | (lbl,b)::q ->
                  unify b (lbl_type lbl kinds);
                  check_args q (lbl_remove lbl kinds)
            in
              begin
                try
                  check_args l p
                with
                    (In_wrong_label lbl) -> raise (Wrong_label (tree.pos,lbl))
              end
        | _ -> ()
      end;
      unify a (ref (Fun_t ((List.map (fun (lbl,b) -> false,lbl,b.kind) l),
                           tree.kind))) ;
      if fulfilled_kind tree.kind then begin
        tree.kind := Ref (defaults_kind tree.kind) ;
        if debug then
          Printf.eprintf "%s: erase > %s\n"
            (print_pos tree.pos) (print_kind tree.kind)
      end
  | Let (name,def,body) -> check def ; check body
  | Fun (proto,body) ->
      let proto_t =
        List.map
          (function
             | lbl,_,kind,None ->   false,lbl,kind
             | lbl,_,kind,Some v -> check v ; unify v kind ; true,lbl,v.kind)
          proto
      in begin
        check body ;
        unify tree (ref (Fun_t (proto_t,body.kind)))
      end
  | FFI (proto,param,f) -> () (* Always fully specified *)

(** Manipulation of values *)

let to_bool t = match t.value with
  | Bool b -> b
  | _ -> assert false

let to_string t = match t.value with
  | String s -> s
  | _ -> assert false

let to_float t = match t.value with
  | Float s -> s
  | _ -> assert false

let to_source t = match t.value with
  | Source s -> s
  | _ -> assert false

let to_request t = match t.value with
  | Request r -> r
  | _ -> assert false

let to_int t = match t.value with
  | Int s -> s
  | _ -> assert false

let to_list t = match t.value with
  | List l -> l
  | _ -> assert false

let to_product t = match t.value with
  | Product (a,b) -> (a,b)
  | _ -> assert false

let to_string_list l = List.map to_string (to_list l)
let to_int_list l = List.map to_int (to_list l)
let to_source_list l = List.map to_source (to_list l)

(** [assoc lbl n l] returns the [n]th element in [l]
  * of which the first component is [lbl]. *)
let rec assoc label n = function
  | [] -> raise Not_found
  | (l,e)::tl ->
       if l = label then
         if n = 1 then
           e
         else
           assoc label (n-1) tl
       else
         assoc label n tl

(** Some shortcuts *)

let int_t = ref Int_t
let unit_t = ref Unit_t
let float_t = ref Float_t
let bool_t = ref Bool_t
let string_t = ref String_t
let source_t = ref Source_t
let request_t = ref Request_t
let list_t t = ref (List_t t)
let product_t a b = ref (Product_t (a,b))
let fun_t p b = ref (Fun_t (p,b))

let mk v = { pos = dummy_pos ; kind = dummy_kind ; value = v }
let unit = mk Unit
let int i = mk (Int i)
let bool i = mk (Bool i)
let float i = mk (Float i)
let string i = mk (String i)
let list l = mk (List l)
let source s = mk (Source s)
let request r = mk (Request r)
let product a b = mk (Product (a,b))
let val_fun p b = mk (Fun (p,b))
let ffi p b = mk (FFI (p,[],b))
let var s = mk (Var s)
let app f x = mk (App (f,x))

(** Error reporting *)

exception Unbound of string
exception Invalid_value of value*string

