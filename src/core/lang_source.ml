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

open Liquidsoap_lang.Lang

let log = Log.make ["lang"]
let metadata_t = list_t (product_t string_t string_t)

let to_metadata_list t =
  let pop v =
    let f (a, b) = (to_string a, to_string b) in
    f (to_product v)
  in
  List.map pop (to_list t)

let to_metadata t = Frame.Metadata.from_list (to_metadata_list t)

let metadata_list m =
  list (List.map (fun (k, v) -> product (string k) (string v)) m)

let metadata m = metadata_list (Frame.Metadata.to_list m)
let metadata_track_t = Format_type.metadata
let track_marks_t = Format_type.track_marks

module Source_val = Liquidsoap_lang.Lang_core.MkAbstract (struct
  type content = Source.source

  let name = "source"
  let descr s = Printf.sprintf "<source#%s>" s#id

  let to_json ~pos _ =
    Runtime_error.raise ~pos
      ~message:(Printf.sprintf "Sources cannot be represented as json")
      "json"

  let compare s1 s2 = Stdlib.compare s1#id s2#id
end)

let source_methods =
  [
    ( "id",
      ([], fun_t [] string_t),
      "Identifier of the source.",
      fun s -> val_fun [] (fun _ -> string s#id) );
    ( "is_ready",
      ([], fun_t [] bool_t),
      "Indicate if a source is ready to stream. This does not mean that the \
       source is currently streaming, just that its resources are all properly \
       initialized.",
      fun (s : Source.source) -> val_fun [] (fun _ -> bool (s#is_ready ())) );
    ( "buffered",
      ([], fun_t [] (list_t (product_t string_t float_t))),
      "Length of buffered data.",
      fun s ->
        val_fun [] (fun _ ->
            let l =
              Frame.Fields.fold
                (fun field _ l ->
                  ( Frame.Fields.string_of_field field,
                    Frame.seconds_of_main
                      (Generator.field_length s#buffer field) )
                  :: l)
                s#content_type []
            in
            list (List.map (fun (lbl, v) -> product (string lbl) (float v)) l))
    );
    ( "last_metadata",
      ([], fun_t [] (nullable_t metadata_t)),
      "Return the last metadata from the source.",
      fun s ->
        val_fun [] (fun _ ->
            match s#last_metadata with None -> null | Some m -> metadata m) );
    ( "on_metadata",
      ([], fun_t [(false, "", fun_t [(false, "", metadata_t)] unit_t)] unit_t),
      "Call a given handler on metadata packets.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_metadata (fun m -> ignore (apply f [("", metadata m)]));
            unit) );
    ( "on_wake_up",
      ([], fun_t [(false, "", fun_t [] unit_t)] unit_t),
      "Register a function to be called after the source is asked to get \
       ready. This is when, for instance, the source's final ID is set.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_wake_up (fun () -> ignore (apply f []));
            unit) );
    ( "on_shutdown",
      ([], fun_t [(false, "", fun_t [] unit_t)] unit_t),
      "Register a function to be called when source shuts down.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_sleep (fun () -> ignore (apply f []));
            unit) );
    ( "on_track",
      ([], fun_t [(false, "", fun_t [(false, "", metadata_t)] unit_t)] unit_t),
      "Call a given handler on new tracks.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_track (fun m -> ignore (apply f [("", metadata m)]));
            unit) );
    ( "remaining",
      ([], fun_t [] float_t),
      "Estimation of remaining time in the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let r = s#remaining in
               if r < 0 then infinity else Frame.seconds_of_main r)) );
    ( "elapsed",
      ([], fun_t [] float_t),
      "Elapsed time in the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let e = s#elapsed in
               if e < 0 then infinity else Frame.seconds_of_main e)) );
    ( "duration",
      ([], fun_t [] float_t),
      "Estimation of the duration of the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let d = s#duration in
               if d < 0 then infinity else Frame.seconds_of_main d)) );
    ( "self_sync",
      ([], fun_t [] bool_t),
      "Is the source currently controlling its own real-time loop.",
      fun s -> val_fun [] (fun _ -> bool (snd s#self_sync)) );
    ( "log",
      ( [],
        record_t
          [
            ( "level",
              method_t
                (fun_t [] (nullable_t int_t))
                [
                  ( "set",
                    ([], fun_t [(false, "", int_t)] unit_t),
                    "Set the source's log level" );
                ] );
          ] ),
      "Get or set the source's log level, from `1` to `5`.",
      fun s ->
        record
          [
            ( "level",
              meth
                (val_fun [] (fun _ ->
                     match s#log#level with Some lvl -> int lvl | None -> null))
                [
                  ( "set",
                    val_fun
                      [("", "", None)]
                      (fun p ->
                        let lvl = min 5 (max 1 (to_int (List.assoc "" p))) in
                        s#log#set_level lvl;
                        unit) );
                ] );
          ] );
    ( "is_up",
      ([], fun_t [] bool_t),
      "Indicate that the source can be asked to produce some data at any time. \
       This is `true` when the source is currently being used or if it could \
       be used at any time, typically inside a `switch` or `fallback`.",
      fun s -> val_fun [] (fun _ -> bool s#is_up) );
    ( "is_active",
      ([], fun_t [] bool_t),
      "`true` if the source is active, i.e. it is continuously animated by its \
       own clock whenever it is ready. Typically, `true` for outputs and \
       sources such as `input.http`.",
      fun s -> val_fun [] (fun _ -> bool s#is_active) );
    ( "seek",
      ([], fun_t [(false, "", float_t)] float_t),
      "Seek forward, in seconds (returns the amount of time effectively \
       seeked).",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            float
              (Frame.seconds_of_main
                 (s#seek (Frame.main_of_seconds (to_float (List.assoc "" p))))))
    );
    ( "skip",
      ([], fun_t [] unit_t),
      "Skip to the next track.",
      fun s ->
        val_fun [] (fun _ ->
            s#abort_track;
            unit) );
    ( "fallible",
      ([], bool_t),
      "Indicate if a source may fail, i.e. may not be ready to stream.",
      fun s -> bool (s#stype = `Fallible) );
    ( "time",
      ([], fun_t [] float_t),
      "Get a source's time, based on its assigned clock.",
      fun s ->
        val_fun [] (fun _ ->
            let ticks =
              if Source.Clock_variables.is_known s#clock then
                (Source.Clock_variables.get s#clock)#get_tick
              else 0
            in
            let frame_position =
              Lazy.force Frame.duration *. float_of_int ticks
            in
            let in_frame_position =
              Frame.seconds_of_main (Frame.position s#cache)
            in
            float (frame_position +. in_frame_position)) );
  ]

let source_methods_t t =
  method_t t (List.map (fun (name, t, doc, _) -> (name, t, doc)) source_methods)

let source_t ?(methods = false) frame_t =
  let t =
    Type.make
      (Type.Constr
         (* The type has to be invariant because we don't want the sup mechanism to be used here, see #2806. *)
         { Type.constructor = "source"; params = [(`Invariant, frame_t)] })
  in
  if methods then source_methods_t t else t

let of_source_t t =
  match (Type.demeth t).Type.descr with
    | Type.Constr { Type.constructor = "source"; params = [(_, t)] } -> t
    | _ -> assert false

let source_tracks_t frame_t =
  Type.meth "track_marks"
    ([], Format_type.track_marks)
    (Type.meth "metadata" ([], Format_type.metadata) frame_t)

let source_tracks s =
  meth unit
    (( Frame.Fields.string_of_field Frame.Fields.metadata,
       Track.to_value (Frame.Fields.metadata, s) )
    :: ( Frame.Fields.string_of_field Frame.Fields.track_marks,
         Track.to_value (Frame.Fields.track_marks, s) )
    :: List.map
         (fun (field, _) ->
           (Frame.Fields.string_of_field field, Track.to_value (field, s)))
         (Frame.Fields.bindings s#content_type))

let source_methods ~base s =
  meth base (List.map (fun (name, _, _, fn) -> (name, fn s)) source_methods)

let source s = source_methods ~base:(Source_val.to_value s) s
let track = Track.to_value ?pos:None
let to_source = Source_val.of_value
let to_source_list l = List.map to_source (to_list l)
let to_track = Track.of_value

(** A method: name, type scheme, documentation and implementation (which takes
    the currently defined source as argument). *)
type 'a operator_method = string * scheme * string * ('a -> value)

(** Ensure that the frame contents of all the sources occurring in the value agree with [t]. *)
let check_content v t =
  let checked_values = ref [] in
  let check t t' = Typing.(t <: t') in
  let rec check_value v t =
    if not (List.memq v !checked_values) then (
      (* We need to avoid checking the same value multiple times, otherwise we
         get an exponential blowup, see #1247. *)
      checked_values := v :: !checked_values;
      match (v.Value.value, (Type.deref t).Type.descr) with
        | _, Type.Var _ -> ()
        | _ when Source_val.is_value v ->
            let source_t = source_t (Source_val.of_value v)#frame_type in
            check source_t t
        | _ when Track.is_value v ->
            let field, s = Track.of_value v in
            if
              field <> Frame.Fields.track_marks
              && field <> Frame.Fields.metadata
            then (
              let t =
                Frame_type.make (Type.var ())
                  (Frame.Fields.add field t Frame.Fields.empty)
              in
              check s#frame_type t)
        | _ when Lang_encoder.V.is_value v ->
            let content_t =
              Encoder.type_of_format (Lang_encoder.V.of_value v)
            in
            let frame_t = Frame_type.make unit_t content_t in
            let encoder_t = Lang_encoder.L.format_t frame_t in
            check encoder_t t
        | Value.Ground _, _ -> ()
        | Value.List l, Type.List { Type.t } ->
            List.iter (fun v -> check_value v t) l
        | Value.Tuple l, Type.Tuple t -> List.iter2 check_value l t
        | Value.Null, _ -> ()
        (* Value can have more methods than the type requires so check from the type here. *)
        | _, Type.Meth _ ->
            let meths, v = Value.split_meths v in
            let meths_t, t = Type.split_meths t in
            List.iter
              (fun { Type.meth; optional; scheme = s } ->
                let t = Typing.instantiate ~level:(-1) s in
                try check_value (List.assoc meth meths) t
                with Not_found when optional -> ())
              meths_t;
            check_value v t
        (* We don't check functions, assuming anything creating a source is a
           FFI registered via add_operator so the check will happen there. *)
        | Fun _, _ | FFI _, _ -> ()
        | _ ->
            failwith
              ("Unhandled value in check_content: " ^ Value.to_string v ^ "."))
  in
  check_value v t

(** An operator is a builtin function that builds a source.
  * It is registered using the wrapper [add_operator].
  * Creating the associated function type (and function) requires some work:
  *  - Specify which content_kind the source will carry:
  *    a given fixed number of channels, any fixed, a variable number?
  *  - The content_kind can also be linked to a type variable,
  *    e.g. the parameter of a format type.
  * From this high-level description a type is created. Often it will
  * carry a type constraint.
  * Once the type has been inferred, the function might be executed,
  * and at this point the type might still not be known completely
  * so we have to force its value within the acceptable range. *)

let _meth = meth

let check_arguments ~env ~return_t arguments =
  (* Create a fresh instantiation of the return type and the type of arguments. *)
  let return_t, arguments =
    let s =
      (* TODO: level -1 generalization is abusive, but it should be a good enough approximation for now *)
      Typing.generalize ~level:(-1)
        (Type.make
           (Type.Tuple (return_t :: List.map (fun (_, t, _, _) -> t) arguments)))
    in
    let t = Typing.instantiate ~level:(-1) s in
    match t.Type.descr with
      | Type.Tuple (return_t :: arguments_t) ->
          ( return_t,
            List.map2
              (fun (name, _, _, _) typ -> (name, typ))
              arguments arguments_t )
      | _ -> assert false
  in
  let arguments =
    List.stable_sort (fun (l, _) (l', _) -> Stdlib.compare l l') arguments
  in
  (* Negotiate content for all sources and formats in the arguments. *)
  let () =
    let env =
      List.stable_sort
        (fun (l, _) (l', _) -> Stdlib.compare l l')
        (List.filter
           (fun (lbl, _) -> lbl <> Liquidsoap_lang.Lang_core.pos_var)
           env)
    in
    List.iter2
      (fun (name, typ) (name', v) ->
        assert (name = name');
        check_content v typ)
      arguments env
  in
  return_t

let add_operator ~(category : Doc.Value.source) ~descr ?(flags = [])
    ?(meth = ([] : 'a operator_method list)) ?base name arguments ~return_t f =
  let compare (x, _, _, _) (y, _, _, _) =
    match (x, y) with
      | "", "" -> 0
      | _, "" -> -1
      | "", _ -> 1
      | x, y -> Stdlib.compare x y
  in
  let arguments =
    ( "id",
      nullable_t string_t,
      Some null,
      Some "Force the value of the source ID." )
    :: List.stable_sort compare arguments
  in
  let f env =
    let pos =
      match Liquidsoap_lang.Lang_core.pos env with
        | [] -> None
        | p :: _ -> Some p
    in
    let return_t = check_arguments ~return_t ~env arguments in
    let src : < Source.source ; .. > = f env in
    src#set_pos pos;
    Typing.(src#frame_type <: return_t);
    ignore
      (Option.map
         (fun id -> src#set_id id)
         (to_valued_option to_string (List.assoc "id" env)));
    let v =
      let src = (src :> Source.source) in
      if category = `Output then source_methods ~base:unit src else source src
    in
    _meth v (List.map (fun (name, _, _, fn) -> (name, fn src)) meth)
  in
  let base_t =
    if category = `Output then unit_t else source_t ~methods:false return_t
  in
  let return_t = source_methods_t base_t in
  let return_t =
    method_t return_t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  let category = `Source category in
  add_builtin ~category ~descr ~flags ?base name arguments return_t f

let add_track_operator ~(category : Doc.Value.source) ~descr ?(flags = [])
    ?(meth = ([] : 'a operator_method list)) ?base name arguments ~return_t f =
  let arguments =
    ( "id",
      nullable_t string_t,
      Some null,
      Some "Force the value of the track ID." )
    :: arguments
  in
  let f env =
    let pos =
      match Liquidsoap_lang.Lang_core.pos env with
        | [] -> None
        | p :: _ -> Some p
    in
    let return_t = check_arguments ~return_t ~env arguments in
    let field, (src : < Source.source ; .. >) = f env in
    src#set_pos pos;
    (if field <> Frame.Fields.track_marks && field <> Frame.Fields.metadata then
       Typing.(
         src#frame_type
         <: method_t (univ_t ())
              [(Frame.Fields.string_of_field field, ([], return_t), "")]));
    ignore
      (Option.map
         (fun id -> src#set_id id)
         (to_valued_option to_string (List.assoc "id" env)));
    let v = Track.to_value (field, (src :> Source.source)) in
    _meth v (List.map (fun (name, _, _, fn) -> (name, fn src)) meth)
  in
  let return_t =
    method_t return_t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  let category = `Track category in
  add_builtin ~category ~descr ~flags ?base name arguments return_t f

let iter_sources ?(on_imprecise = fun () -> ()) f v =
  let itered_values = ref [] in
  let rec iter_term env v =
    let iter_base_term env v =
      match v.Term.term with
        | `Ground _ | `Encoder _ -> ()
        | `List l -> List.iter (iter_term env) l
        | `Tuple l -> List.iter (iter_term env) l
        | `Null -> ()
        | `Cast (a, _) -> iter_term env a
        | `Invoke { Term.invoked = a } -> iter_term env a
        | `Open (a, b) ->
            iter_term env a;
            iter_term env b
        | `Let { Term.def = a; body = b; _ } | `Seq (a, b) ->
            iter_term env a;
            iter_term env b
        | `Var v -> (
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
        | `App (a, l) ->
            iter_term env a;
            List.iter (fun (_, v) -> iter_term env v) l
        | `Fun { Term.arguments; body } | `RFun (_, { Term.arguments; body }) ->
            iter_term env body;
            List.iter
              (function
                | { Term.default = Some v } -> iter_term env v | _ -> ())
              arguments
    in
    Term.Methods.iter
      (fun _ meth_term -> iter_term env meth_term)
      v.Term.methods;
    iter_base_term env v
  and iter_value v =
    if not (List.memq v !itered_values) then (
      (* We need to avoid checking the same value multiple times, otherwise we
         get an exponential blowup, see #1247. *)
      itered_values := v :: !itered_values;
      Value.Methods.iter (fun _ v -> iter_value v) v.Value.methods;
      match v.value with
        | _ when Source_val.is_value v -> f (Source_val.of_value v)
        | Ground _ -> ()
        | List l -> List.iter iter_value l
        | Tuple l -> List.iter iter_value l
        | Null -> ()
        | Fun (proto, env, body) ->
            (* The following is necessarily imprecise: we might see sources that
               will be unused in the execution of the function. *)
            iter_term env body;
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
        | FFI (proto, _) ->
            on_imprecise ();
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
      (*
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
                    | _ when Source_val.is_value v -> true
                    | Ground _ | Null -> false
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
                         are not sharing sources contained in references!"))
               *))
  in
  iter_value v

let iter_sources = iter_sources
