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

include Value
module Ground = Term.Ground
open Ground

type t = Type.t
type scheme = Type.scheme
type value = Value.t = { pos : Pos.Option.t; value : in_value }

let log = Log.make ["lang"]

(** Type construction *)

let ground_t x = Type.make (Type.Ground x)
let int_t = ground_t Type.Int
let unit_t = Type.make Type.unit
let float_t = ground_t Type.Float
let bool_t = ground_t Type.Bool
let string_t = ground_t Type.String
let tuple_t l = Type.make (Type.Tuple l)
let product_t a b = tuple_t [a; b]

let rec record_t = function
  | [] -> unit_t
  | (l, t) :: r -> Type.meth l ([], t) (record_t r)

let rec method_t t0 = function
  | [] -> t0
  | (l, t, doc) :: r -> Type.meth l t ~doc (method_t t0 r)

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
let metadata_t = list_t (product_t string_t string_t)
let univ_t ?(constraints = []) () = Type.var ~constraints ()
let getter_t a = Type.make (Type.Getter a)
let frame_kind_t ~audio ~video ~midi = Term.frame_kind_t audio video midi
let of_frame_kind_t t = Term.of_frame_kind_t t
let source_t t = Term.source_t t
let of_source_t t = Term.of_source_t t
let format_t t = Term.format_t t
let kind_t k = Term.kind_t k
let kind_none_t = Term.kind_t Frame.none
let empty = { Frame.audio = Frame.none; video = Frame.none; midi = Frame.none }
let any = { Frame.audio = `Any; video = `Any; midi = `Any }
let internal = { Frame.audio = `Internal; video = `Internal; midi = `Internal }
let audio_pcm = { Frame.audio = Frame.audio_pcm; video = `Any; midi = `Any }

let audio_params p =
  {
    Frame.audio = `Format (Content.Audio.lift_params p);
    video = `Any;
    midi = `Any;
  }

let audio_n n = { Frame.audio = Frame.audio_n n; video = `Any; midi = `Any }
let audio_mono = audio_params { Content.Contents.channel_layout = lazy `Mono }

let audio_stereo =
  audio_params { Content.Contents.channel_layout = lazy `Stereo }

let video_yuva420p =
  { Frame.audio = `Any; video = Frame.video_yuva420p; midi = `Any }

let midi = { Frame.audio = `Any; video = `Any; midi = Frame.midi_native }

let midi_n n =
  {
    Frame.audio = `Any;
    video = `Any;
    midi = `Format (Content.Midi.lift_params { Content.Contents.channels = n });
  }

let kind_type_of_kind_format fields =
  let audio = Term.kind_t fields.Frame.audio in
  let video = Term.kind_t fields.Frame.video in
  let midi = Term.kind_t fields.Frame.midi in
  frame_kind_t ~audio ~video ~midi

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
let source s = mk (Source s)
let reference x = mk (Ref x)
let val_fun p f = mk (FFI (p, f))

let val_cst_fun p c =
  let p = List.map (fun (l, d) -> (l, "_", d)) p in
  let f t tm = mk (Fun (p, [], { Term.t; Term.term = tm })) in
  let mkg t = Type.make (Type.Ground t) in
  (* Convert the value into a term if possible, to enable introspection, mostly
     for printing. *)
  match c.value with
    | Tuple [] -> f (Type.make Type.unit) Term.unit
    | Ground (Int i) -> f (mkg Type.Int) (Term.Ground (Term.Ground.Int i))
    | Ground (Bool i) -> f (mkg Type.Bool) (Term.Ground (Term.Ground.Bool i))
    | Ground (Float i) -> f (mkg Type.Float) (Term.Ground (Term.Ground.Float i))
    | Ground (String i) ->
        f (mkg Type.String) (Term.Ground (Term.Ground.String i))
    | _ -> mk (FFI (p, fun _ -> c))

let metadata m =
  list (Hashtbl.fold (fun k v l -> product (string k) (string v) :: l) m [])

(** Helpers for defining protocols. *)

let to_proto_doc ~syntax ~static doc =
  let item = new Doc.item ~sort:false doc in
  item#add_subsection "syntax" (Lazy.from_val (Doc.trivial syntax));
  item#add_subsection "static"
    (Lazy.from_val (Doc.trivial (string_of_bool static)));
  item

let add_protocol ~syntax ~doc ~static name resolver =
  let doc () = to_proto_doc ~syntax ~static doc in
  let doc = Lazy.from_fun doc in
  let spec = { Request.static; resolve = resolver } in
  Request.protocols#register ~doc name spec

(** Helpers for defining builtin functions. *)

type proto = (string * t * value option * string option) list

let doc_of_prototype_item ~generalized t d doc =
  let doc = match doc with None -> "(no doc)" | Some d -> d in
  let item = new Doc.item doc in
  item#add_subsection "type"
    (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
  item#add_subsection "default"
    (match d with
      | None -> Lazy.from_val (Doc.trivial "None")
      | Some d -> Lazy.from_fun (fun () -> Doc.trivial (Value.to_string d)));
  item

let builtin_type p t =
  Type.make
    (Type.Arrow (List.map (fun (lbl, t, opt, _) -> (opt <> None, lbl, t)) p, t))

let to_plugin_doc category flags examples main_doc proto return_t =
  let item = new Doc.item ~sort:false main_doc in
  let meths, return_t = Type.split_meths return_t in
  let t = builtin_type proto return_t in
  let generalized = Type.filter_vars (fun _ -> true) t in
  item#add_subsection "_category"
    (Lazy.from_fun (fun () ->
         Doc.trivial (Documentation.string_of_category category)));
  item#add_subsection "_type"
    (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
  List.iter
    (fun f ->
      item#add_subsection "_flag"
        (Lazy.from_fun (fun () -> Doc.trivial (Documentation.string_of_flag f))))
    flags;
  if meths <> [] then
    item#add_subsection "_methods"
      (Lazy.from_fun (fun () -> Repr.doc_of_meths meths));
  List.iter
    (fun e ->
      let e =
        if e.[0] = '\n' then String.sub e 1 (String.length e - 1) else e
      in
      let e =
        if e.[String.length e - 1] = '\n' then
          String.sub e 0 (String.length e - 1)
        else e
      in
      item#add_subsection "_example" (Lazy.from_fun (fun () -> new Doc.item e)))
    examples;
  List.iter
    (fun (l, t, d, doc) ->
      item#add_subsection
        (if l = "" then "(unlabeled)" else l)
        (Lazy.from_fun (fun () -> doc_of_prototype_item ~generalized t d doc)))
    proto;
  item

let meth_fun = meth

let add_builtin ~category ~descr ?(flags = []) ?(meth = []) ?(examples = [])
    name proto return_t f =
  let return_t =
    if meth = [] then return_t
    else (
      let meth = List.map (fun (l, t, d, _) -> (l, t, d)) meth in
      method_t return_t meth)
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
  let generalized = Type.filter_vars (fun _ -> true) t in
  let doc () = to_plugin_doc category flags examples descr proto return_t in
  let doc = Lazy.from_fun doc in
  Environment.add_builtin ~doc
    (String.split_on_char '.' name)
    ((generalized, t), value)

let add_builtin_base ~category ~descr ?(flags = []) name value t =
  let value = { pos = t.Type.pos; value } in
  let generalized = Type.filter_vars (fun _ -> true) t in
  let doc () =
    let doc = new Doc.item ~sort:false descr in
    doc#add_subsection "_category"
      (Lazy.from_fun (fun () ->
           Doc.trivial (Documentation.string_of_category category)));
    doc#add_subsection "_type"
      (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
    List.iter
      (fun f ->
        doc#add_subsection "_flag"
          (Lazy.from_fun (fun () ->
               Doc.trivial (Documentation.string_of_flag f))))
      flags;
    doc
  in
  Environment.add_builtin ~doc:(Lazy.from_fun doc)
    (String.split_on_char '.' name)
    ((generalized, t), value)

let add_module name = Environment.add_module (String.split_on_char '.' name)

let iter_sources ?on_reference ~static_analysis_failed f v =
  let itered_values = ref [] in
  let rec iter_term env v =
    match v.Term.term with
      | Term.Ground _ | Term.Encoder _ -> ()
      | Term.List l -> List.iter (iter_term env) l
      | Term.Tuple l -> List.iter (iter_term env) l
      | Term.Null -> ()
      | Term.Cast (a, _) -> iter_term env a
      | Term.Meth (_, a, b) ->
          iter_term env a;
          iter_term env b
      | Term.Invoke (a, _) -> iter_term env a
      | Term.Open (a, b) ->
          iter_term env a;
          iter_term env b
      | Term.Let { Term.def = a; body = b; _ } | Term.Seq (a, b) ->
          iter_term env a;
          iter_term env b
      | Term.Var v -> (
          try
            (* If it's locally bound it won't be in [env]. *)
            (* TODO since inner-bound variables don't mask outer ones in [env],
             *   we are actually checking values that may be out of reach. *)
            let v = List.assoc v env in
            if Lazy.is_val v then (
              let v = Lazy.force v in
              iter_value v)
            else ()
          with Not_found -> ())
      | Term.App (a, l) ->
          iter_term env a;
          List.iter (fun (_, v) -> iter_term env v) l
      | Term.Fun (_, proto, body) | Term.RFun (_, _, proto, body) ->
          iter_term env body;
          List.iter
            (fun (_, _, _, v) ->
              match v with Some v -> iter_term env v | None -> ())
            proto
  and iter_value v =
    if not (List.memq v !itered_values) then (
      (* We need to avoid checking the same value multiple times, otherwise we
         get an exponential blowup, see #1247. *)
      itered_values := v :: !itered_values;
      match v.value with
        | Source s -> f s
        | Ground _ | Encoder _ -> ()
        | List l -> List.iter iter_value l
        | Tuple l -> List.iter iter_value l
        | Null -> ()
        | Meth (_, a, b) ->
            iter_value a;
            iter_value b
        | Fun (proto, env, body) ->
            (* The following is necessarily imprecise: we might see sources that
               will be unused in the execution of the function. *)
            iter_term env body;
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
        | FFI (proto, _) ->
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
        | Ref r ->
            if List.memq r !static_analysis_failed then ()
            else (
              (* Do not walk inside references, otherwise the list of "contained"
                 sources may change from one time to the next, which makes it
                 impossible to avoid ill-balanced activations. Not walking inside
                 references does not break things more than they are already:
                 detecting sharing in presence of references to sources cannot be
                 done statically anyway. We display a fat log message to warn
                 about this risky situation. *)
              let may_have_source =
                let rec aux v =
                  match v.value with
                    | Source _ -> true
                    | Ground _ | Encoder _ | Null -> false
                    | List l -> List.exists aux l
                    | Tuple l -> List.exists aux l
                    | Ref r -> aux (Atomic.get r)
                    | Fun _ | FFI _ -> true
                    | Meth (_, v, t) -> aux v || aux t
                in
                aux v
              in
              static_analysis_failed := r :: !static_analysis_failed;
              if may_have_source then (
                match on_reference with
                  | Some f -> f ()
                  | None ->
                      log#severe
                        "WARNING! Found a reference, potentially containing \
                         sources, inside a dynamic source-producing function. \
                         Static analysis cannot be performed: make sure you \
                         are not sharing sources contained in references!")))
  in
  iter_value v

let iter_sources = iter_sources ~static_analysis_failed:(ref [])

(* Delay this function in order not to have Lang depend on Evaluation. *)
let apply_fun : (?pos:Pos.t -> value -> env -> value) ref =
  ref (fun ?pos:_ _ -> assert false)

let apply f p = Clock.collect_after (fun () -> !apply_fun f p)

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

let to_source t =
  match (demeth t).value with Source s -> s | _ -> assert false

let to_format t =
  match (demeth t).value with Encoder f -> f | _ -> assert false

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

let to_metadata_list t =
  let pop v =
    let f (a, b) = (to_string a, to_string b) in
    f (to_product v)
  in
  List.map pop (to_list t)

let to_metadata t =
  let t = to_metadata_list t in
  let metas = Hashtbl.create 10 in
  List.iter (fun (a, b) -> Hashtbl.add metas a b) t;
  metas

let to_string_list l = List.map to_string (to_list l)
let to_int_list l = List.map to_int (to_list l)
let to_source_list l = List.map to_source (to_list l)

let to_getter t =
  match (demeth t).value with
    | Fun ([], _, _) | FFI ([], _) -> fun () -> apply t []
    | _ -> fun () -> t

(** [assoc lbl n l] returns the [n]th element in [l]
  * of which the first component is [lbl]. *)
let rec assoc label n = function
  | [] -> raise Not_found
  | (l, e) :: tl ->
      if l = label then if n = 1 then e else assoc label (n - 1) tl
      else assoc label n tl

let raise_error = Runtime_error.raise

let raise_as_runtime ~bt ~kind exn =
  match exn with
    | Runtime_error.Runtime_error _ -> Printexc.raise_with_backtrace exn bt
    | exn ->
        raise_error ~bt ~pos:[]
          ~message:
            (Printf.sprintf "%s\nBacktrace:\n%s" (Printexc.to_string exn)
               (Printexc.raw_backtrace_to_string bt))
          kind

(* This is used to pass position in application environment. *)
module Position = Value.MkAbstract (struct
  type content = Pos.t list

  let name = "position"

  let descr pos =
    Printf.sprintf "position<%s>" (Pos.List.to_string ~newlines:false pos)

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Positions cannot be represented as json"
      "json"

  let compare = Stdlib.compare
end)

let pos_var = "_pos_"
let pos env = Position.of_value (List.assoc pos_var env)
