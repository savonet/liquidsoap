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

module Lang = Liquidsoap_lang.Lang
module Flags = Liquidsoap_lang.Flags
open Lang

let apply ?pos v env =
  let ret = apply ?pos v env in
  Clock.after_eval ();
  ret

module ClockValue = struct
  include Value.MkCustom (struct
    type content = Clock.t

    let name = "clock"
    let to_string = Clock.descr

    let to_json ~pos _ =
      Lang.raise_error ~message:"Clocks cannot be represented as json" ~pos
        "json"

    let compare = Stdlib.compare
  end)

  let base_t = t
  let to_base_value = to_value

  let methods =
    [
      ( "id",
        Lang.ref_t Lang.string_t,
        "The clock's id",
        fun c ->
          let get () = Lang.string (Clock.id c) in
          let set v = Clock.set_id c (Lang.to_string v) in
          Lang.reference get set );
      ( "sync",
        Lang.fun_t [] Lang.string_t,
        "The clock's current sync mode. One of: `\"stopped\"`, `\"stopping\"`, \
         `\"auto\"`, `\"CPU\"`, `\"unsynced\"` or `\"passive\"`.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.string Clock.(string_of_sync_mode (sync c))) );
      ( "start",
        Lang.fun_t [(true, "force", Lang.bool_t)] Lang.unit_t,
        "Start the clock.",
        fun c ->
          Lang.val_fun
            [("force", "force", Some (Lang.bool true))]
            (fun p ->
              let pos = Lang.pos p in
              let force = Lang.to_bool (List.assoc "force" p) in
              try
                Clock.start ~force c;
                Lang.unit
              with Clock.Invalid_state ->
                Runtime_error.raise
                  ~message:
                    (Printf.sprintf "Invalid clock state: %s"
                       Clock.(string_of_sync_mode (sync c)))
                  ~pos "clock") );
      ( "stop",
        Lang.fun_t [] Lang.unit_t,
        "Stop the clock. Does nothing if the clock is stopping or stopped.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Clock.stop c;
              Lang.unit) );
      ( "self_sync",
        Lang.fun_t [] Lang.bool_t,
        "`true` if the clock is in control of its latency.",
        fun c -> Lang.val_fun [] (fun _ -> Lang.bool (Clock.self_sync c)) );
      ( "unify",
        Lang.fun_t [(false, "", base_t)] Lang.unit_t,
        "Unify the clock with another one. One of the two clocks should be in \
         `\"stopped\"` sync mode.",
        fun c ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let pos = match Lang.pos p with p :: _ -> Some p | [] -> None in
              let c' = of_value (List.assoc "" p) in
              Clock.unify ~pos c c';
              Lang.unit) );
      ( "tick",
        Lang.fun_t [] Lang.unit_t,
        "Animate the clock and run one tick",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Clock.tick c;
              Lang.unit) );
      ( "ticks",
        Lang.fun_t [] Lang.int_t,
        "The total number of times the clock has ticked.",
        fun c -> Lang.val_fun [] (fun _ -> Lang.int (Clock.ticks c)) );
    ]

  let t =
    method_t base_t
      (List.map (fun (lbl, typ, descr, _) -> (lbl, ([], typ), descr)) methods)

  let to_value c =
    Lang.meth (to_base_value c)
      (List.map (fun (lbl, _, _, v) -> (lbl, v c)) methods)
end

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

module Source_val = Liquidsoap_lang.Lang_core.MkCustom (struct
  type content = Source.source

  let name = "source"

  let to_string s =
    Printf.sprintf "<source(id=%s, frame_type=%s>" s#id
      (Type.to_string s#frame_type)

  let to_json ~pos _ =
    Runtime_error.raise ~pos
      ~message:(Printf.sprintf "Sources cannot be represented as json")
      "json"

  let compare s1 s2 = Stdlib.compare s1#id s2#id
end)

type callback_param = { name : string; typ : t; default : value option }

type 'a callback = {
  name : string;
  params : callback_param list;
  descr : string;
  register_deprecated_argument : bool;
  arg_t : (bool * string * t) list;
  register : params:(string * value) list -> 'a -> (env -> unit) -> unit;
}

let callback { name; params; descr; arg_t; register } =
  {
    name;
    scheme =
      ( [],
        fun_t
          (List.map
             (fun { name; typ; default } -> (default <> None, name, typ))
             params
          @ [
              (false, "synchronous", Lang.bool_t);
              (false, "", fun_t arg_t unit_t);
            ])
          unit_t );
    descr = Printf.sprintf "Call a given handler %s" descr;
    value =
      (fun s ->
        val_fun
          ([("synchronous", "synchronous", None); ("", "", None)]
          @ List.map (fun { name; default } -> (name, name, default)) params)
          (fun p ->
            let synchronous = Lang.to_bool (List.assoc "synchronous" p) in
            let fn = assoc "" 1 p in
            let fn args = ignore (apply fn args) in
            let fn =
              if synchronous then fn
              else fun args ->
                let task =
                  {
                    Duppy.Task.priority = `Maybe_blocking;
                    events = [`Delay 0.];
                    handler =
                      (fun _ ->
                        fn args;
                        []);
                  }
                in
                Duppy.Task.add Tutils.scheduler task
            in
            (s#log : Log.t)#debug "Registering %s %s callback" name
              (if synchronous then "synchronous" else "asynchronous");
            register ~params:p s fn;
            unit));
  }

let source_callbacks =
  [
    {
      name = "on_metadata";
      params = [];
      descr = "to execute on each metadata";
      register_deprecated_argument = false;
      arg_t = [(false, "", metadata_t)];
      register =
        (fun ~params:_ s f ->
          let f m = f [("", metadata m)] in
          s#on_frame (`Metadata f));
    };
    {
      name = "on_wake_up";
      descr = "to be called after the source is asked to get ready";
      params = [];
      register_deprecated_argument = false;
      arg_t = [];
      register = (fun ~params:_ s f -> s#on_wake_up (fun () -> f []));
    };
    {
      name = "on_shutdown";
      params = [];
      descr = "to be called when source shuts down";
      register_deprecated_argument = false;
      arg_t = [];
      register = (fun ~params:_ s f -> s#on_sleep (fun () -> f []));
    };
    {
      name = "on_track";
      params = [];
      descr = "on track marks";
      register_deprecated_argument = false;
      arg_t = [(false, "", metadata_t)];
      register =
        (fun ~params:_ s f ->
          let f m = f [("", metadata m)] in
          s#on_frame (`Track f));
    };
    {
      name = "on_frame";
      params =
        [
          { name = "before"; typ = Lang.bool_t; default = Some (Lang.bool true) };
        ];
      descr =
        "on frame. When `before` is `true`, callback is executed before \
         computing the frame and after otherwise";
      register_deprecated_argument = false;
      arg_t = [];
      register =
        (fun ~params:p s on_frame ->
          let on_frame () = on_frame [] in
          let before = Lang.to_bool (List.assoc "before" p) in
          s#on_frame (`Frame { Source.before; on_frame }));
    };
    {
      name = "on_position";
      params =
        [
          { name = "position"; typ = Lang.(getter_t float_t); default = None };
          {
            name = "remaining";
            typ = Lang.bool_t;
            default = Some (Lang.bool false);
          };
          {
            name = "allow_partial";
            typ = Lang.bool_t;
            default = Some (Lang.bool true);
          };
        ];
      descr =
        "on track position. If `remaining` is `false`, callback is executed \
         when position in track is more or equal to `position`. If `remaining` \
         is `true`, callback is executed when remaining time in the current \
         track is less or equal to `position`. Keep in mind that elapsed time \
         is exact while remaining time is always estimated. Remaining time is \
         usually more accurate for file-based sources. When `allow_partial` is \
         `true`, if the current track ends before the `offset` position is \
         reached, callback is still executed";
      register_deprecated_argument = false;
      arg_t = [(false, "", float_t); (false, "", metadata_t)];
      register =
        (fun ~params:p s on_position ->
          let on_position ~pos m =
            on_position [("", float pos); ("", metadata m)]
          in
          let allow_partial = Lang.to_bool (List.assoc "allow_partial" p) in
          let remaining = Lang.to_bool (List.assoc "remaining" p) in
          let mode = if remaining then `Remaining else `Elapsed in
          let position = Lang.to_float_getter (List.assoc "position" p) in
          let position () = Frame.main_of_seconds (position ()) in
          s#on_frame
            (`Position
               {
                 Source.allow_partial;
                 position;
                 mode;
                 executed = false;
                 on_position;
               }));
    };
  ]

let source_methods =
  [
    {
      name = "id";
      scheme = ([], fun_t [] string_t);
      descr = "Identifier of the source.";
      value = (fun s -> val_fun [] (fun _ -> string s#id));
    };
    {
      name = "is_ready";
      scheme = ([], fun_t [] bool_t);
      descr =
        "Indicate if a source is ready to stream. This does not mean that the \
         source is currently streaming, just that its resources are all \
         properly initialized.";
      value = (fun s -> val_fun [] (fun _ -> bool s#is_ready));
    };
    {
      name = "insert_metadata";
      scheme =
        ( [],
          Lang.fun_t
            [(true, "new_track", Lang.bool_t); (false, "", metadata_t)]
            Lang.unit_t );
      descr =
        "Dynamically insert metadata in a stream. Inserts a new track with the \
         given metadata if `new_track` is `true`.";
      value =
        (fun s ->
          Lang.val_fun
            [("new_track", "new_track", Some (Lang.bool false)); ("", "", None)]
            (fun p ->
              let new_track = to_bool (List.assoc "new_track" p) in
              let m = to_metadata (List.assoc "" p) in
              s#insert_metadata ~new_track m;
              unit));
    };
    {
      name = "reset_last_metadata_on_track";
      scheme = ([], ref_t bool_t);
      descr =
        "If `true`, the source's `last_metadata` is reset on each new track. \
         If a metadata is present along with the track mark, then it becomes \
         the new `last_metadata`, otherwise, `last_metadata becomes `null`.";
      value =
        (fun s ->
          reference
            (fun () -> bool s#reset_last_metadata_on_track)
            (fun b -> s#set_reset_last_metadata_on_track (to_bool b)));
    };
    {
      name = "buffered";
      scheme = ([], fun_t [] (list_t (product_t string_t float_t)));
      descr = "Length of buffered data.";
      value =
        (fun s ->
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
              list (List.map (fun (lbl, v) -> product (string lbl) (float v)) l)));
    };
    {
      name = "last_metadata";
      scheme = ([], fun_t [] (nullable_t metadata_t));
      descr = "Return the last metadata from the source.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              match s#last_metadata with
                | None -> null
                | Some (_, m) -> metadata m));
    };
    {
      name = "register_command";
      scheme =
        ( [],
          fun_t
            [
              (true, "usage", Lang.nullable_t Lang.string_t);
              (false, "description", Lang.string_t);
              (false, "", Lang.string_t);
              (false, "", Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t);
            ]
            unit_t );
      descr =
        "Register a server command for this source. Command is registered \
         under the source's id namespace when it gets up and de-registered \
         when it gets down.";
      value =
        (fun s ->
          val_fun
            [
              ("usage", "usage", Some Lang.null);
              ("description", "description", None);
              ("", "", None);
              ("", "", None);
            ]
            (fun p ->
              let usage =
                Lang.to_valued_option Lang.to_string (List.assoc "usage" p)
              in
              let descr = Lang.to_string (List.assoc "description" p) in
              let command = Lang.to_string (Lang.assoc "" 1 p) in
              let f = Lang.assoc "" 2 p in
              let f x = Lang.to_string (apply f [("", Lang.string x)]) in
              s#register_command ?usage ~descr command f;
              unit));
    };
    {
      name = "remaining";
      scheme = ([], fun_t [] float_t);
      descr = "Estimation of remaining time in the current track.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              float
                (let r = s#remaining in
                 if r < 0 then infinity else Frame.seconds_of_main r)));
    };
    {
      name = "elapsed";
      scheme = ([], fun_t [] float_t);
      descr = "Elapsed time in the current track.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              float
                (let e = s#elapsed in
                 if e < 0 then infinity else Frame.seconds_of_main e)));
    };
    {
      name = "duration";
      scheme = ([], fun_t [] float_t);
      descr = "Estimation of the duration of the current track.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              float
                (let d = s#duration in
                 if d < 0 then infinity else Frame.seconds_of_main d)));
    };
    {
      name = "self_sync";
      scheme = ([], fun_t [] bool_t);
      descr = "Is the source currently controlling its own real-time loop.";
      value = (fun s -> val_fun [] (fun _ -> bool (snd s#self_sync <> None)));
    };
    {
      name = "log";
      scheme = ([], record_t [("level", ref_t int_t)]);
      descr = "Get or set the source's log level, from `1` to `5`.";
      value =
        (fun s ->
          record
            [
              ( "level",
                reference
                  (fun () -> int s#log#level)
                  (fun v -> s#log#set_level (to_int v)) );
            ]);
    };
    {
      name = "is_up";
      scheme = ([], fun_t [] bool_t);
      descr =
        "Indicate that the source can be asked to produce some data at any \
         time. This is `true` when the source is currently being used or if it \
         could be used at any time, typically inside a `switch` or `fallback`.";
      value = (fun s -> val_fun [] (fun _ -> bool s#is_up));
    };
    {
      name = "is_active";
      scheme = ([], fun_t [] bool_t);
      descr =
        "`true` if the source is active, i.e. it is continuously animated by \
         its own clock whenever it is ready. Typically, `true` for outputs and \
         sources such as `input.http`.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              bool (match s#source_type with `Passive -> false | _ -> true)));
    };
    {
      name = "seek";
      scheme = ([], fun_t [(false, "", float_t)] float_t);
      descr =
        "Seek forward, in seconds (returns the amount of time effectively \
         seeked).";
      value =
        (fun s ->
          val_fun
            [("", "", None)]
            (fun p ->
              float
                (Frame.seconds_of_main
                   (s#seek (Frame.main_of_seconds (to_float (List.assoc "" p)))))));
    };
    {
      name = "skip";
      scheme = ([], fun_t [] unit_t);
      descr = "Skip to the next track.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              s#abort_track;
              unit));
    };
    {
      name = "fallible";
      scheme = ([], bool_t);
      descr = "Indicate if a source may fail, i.e. may not be ready to stream.";
      value = (fun s -> bool s#fallible);
    };
    {
      name = "clock";
      scheme = ([], ClockValue.base_t);
      descr = "The source's clock";
      value = (fun s -> ClockValue.to_base_value s#clock);
    };
    {
      name = "time";
      scheme = ([], fun_t [] float_t);
      descr = "Get a source's time, based on its assigned clock.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              let ticks = Clock.ticks s#clock in
              let frame_position =
                Lazy.force Frame.duration *. float_of_int ticks
              in
              let in_frame_position =
                if s#is_ready then
                  Frame.(seconds_of_main (position s#get_frame))
                else 0.
              in
              float (frame_position +. in_frame_position)));
    };
  ]

let source_methods =
  List.map (fun m -> (m, `Method)) source_methods
  @ List.map (fun c -> (callback c, `Callback)) source_callbacks

let make_t ?pos = Type.make ?pos:(Option.map Pos.of_lexing_pos pos)

let _method_t t l =
  List.fold_left
    (fun t ({ name; scheme; descr = doc }, category) ->
      Type.meth ~doc ~category name scheme t)
    t l

let source_methods_t t = _method_t t source_methods

let source_t ?(pos : Liquidsoap_lang.Term_base.parsed_pos option)
    ?(methods = false) frame_t =
  let t =
    make_t ?pos
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

let source_methods ~base s =
  meth base
    (List.map (fun ({ name; value }, _) -> (name, value s)) source_methods)

let source s = source_methods ~base:(Source_val.to_value s) s
let track = Track.to_value ?pos:None
let to_source = Source_val.of_value
let to_source_list l = List.map to_source (to_list l)
let to_track = Track.of_value

let has_value_flag v flag =
  match v with
    | Value.Float _ | Value.String _ | Value.Bool _ | Value.Null _ -> true
    | v -> Value.has_flag v flag

let add_value_flag v flag =
  match v with
    | Value.Float _ | Value.String _ | Value.Bool _ | Value.Null _ -> ()
    | v -> Value.add_flag v flag

(** Ensure that the frame contents of all the sources occurring in the value
    agree with [t]. *)
let check_content v t =
  let check t t' = Typing.(t <: t') in
  let rec check_value v t =
    if not (has_value_flag v Flags.checked_value) then (
      add_value_flag v Flags.checked_value;
      match (v, (Type.deref t).Type.descr) with
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
        | Value.Int _, _
        | Value.Float _, _
        | Value.String _, _
        | Value.Bool _, _
        | Value.Custom _, _ ->
            ()
        | Value.List { value = l }, Type.List { Type.t } ->
            List.iter (fun v -> check_value v t) l
        | Value.Tuple { value = l }, Type.Tuple t -> List.iter2 check_value l t
        | Value.Null _, _ -> ()
        | _, Type.Nullable t -> check_value v t
        (* Value can have more methods than the type requires so check from the type here. *)
        | _, Type.Meth _ ->
            let meths, v = Value.split_meths v in
            let meths_t, t = Type.split_meths t in
            List.iter
              (fun { Type.meth; optional; scheme = generalized, t } ->
                let names = List.map (fun v -> v.Type.name) generalized in
                let handler =
                  Type.Fresh.init
                    ~selector:(fun v -> List.mem v.Type.name names)
                    ()
                in
                let t = Type.Fresh.make handler t in
                try check_value (List.assoc meth meths) t
                with Not_found when optional -> ())
              meths_t;
            check_value v t
        | Value.Fun { fun_args = []; fun_body = ret }, Type.Getter t ->
            Typing.(ret.Term.t <: t)
        | Value.FFI ({ ffi_args = []; ffi_fn } as ffi), Type.Getter t ->
            ffi.ffi_fn <-
              (fun env ->
                let v = ffi_fn env in
                check_value v t;
                v)
        | ( Value.Fun { fun_args = args; fun_body = ret },
            Type.Arrow (args_t, ret_t) ) ->
            List.iter
              (fun typ ->
                match typ with
                  | true, lbl_t, typ ->
                      List.iter
                        (fun arg ->
                          match arg with
                            | lbl, _, Some v when lbl = lbl_t ->
                                check_value v typ
                            | _ -> ())
                        args
                  | _ -> ())
              args_t;
            Typing.(ret.Term.t <: ret_t)
        | Value.FFI ({ ffi_args; ffi_fn } as ffi), Type.Arrow (args_t, ret_t) ->
            List.iter
              (fun typ ->
                match typ with
                  | true, lbl_t, typ ->
                      List.iter
                        (fun arg ->
                          match arg with
                            | lbl, _, Some v when lbl = lbl_t ->
                                check_value v typ
                            | _ -> ())
                        ffi_args
                  | _ -> ())
              args_t;
            ffi.ffi_fn <-
              (fun env ->
                let v = ffi_fn env in
                check_value v ret_t;
                v)
        | _ ->
            failwith
              (Printf.sprintf "Unhandled value in check_content: %s, type: %s."
                 (Value.to_string v) (Type.to_string t)))
  in
  check_value v t

(** An operator is a builtin function that builds a source. It is registered
    using the wrapper [add_operator]. Creating the associated function type (and
    function) requires some work:
    - Specify which content_kind the source will carry: a given fixed number of
      channels, any fixed, a variable number?
    - The content_kind can also be linked to a type variable, e.g. the parameter
      of a format type.

    From this high-level description a type is created. Often it will carry a
    type constraint.

    Once the type has been inferred, the function might be executed, and at this
    point the type might still not be known completely so we have to force its
    value within the acceptable range. *)

let _meth = meth

let check_arguments ~env ~return_t arguments =
  let handler = Type.Fresh.init () in
  let return_t = Type.Fresh.make handler return_t in
  let arguments =
    List.map (fun (lbl, t, _, _) -> (lbl, Type.Fresh.make handler t)) arguments
  in
  let arguments =
    List.stable_sort (fun (l, _) (l', _) -> Stdlib.compare l l') arguments
  in
  (* Generalize all terms inside the arguments *)
  let map =
    let open Liquidsoap_lang.Value in
    let rec map v =
      let v =
        match v with
          | Int _ | Float _ | String _ | Bool _ | Custom _ | Null _ -> v
          | List ({ value = l } as v) -> List { v with value = List.map map l }
          | Tuple ({ value = l } as v) ->
              Tuple { v with value = List.map map l }
          | Fun ({ fun_args = args; fun_body = ret } as fun_v) ->
              Fun
                {
                  fun_v with
                  fun_args =
                    List.map (fun (l, l', v) -> (l, l', Option.map map v)) args;
                  fun_body = Term.fresh ~handler ret;
                }
          | FFI ffi ->
              FFI
                {
                  ffi with
                  ffi_args =
                    List.map
                      (fun (l, l', v) -> (l, l', Option.map map v))
                      ffi.ffi_args;
                  ffi_fn =
                    (fun env ->
                      let v = ffi.ffi_fn env in
                      map v);
                }
      in
      map_methods v (Methods.map map)
    in
    map
  in
  let env = List.map (fun (lbl, v) -> (lbl, map v)) env in
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
  (return_t, env)

let deprecated_callback_registration_arguments callbacks =
  match !Liquidsoap_lang.Runtime.deprecated with
    | false -> ([], [])
    | true ->
        List.fold_left
          (fun (arguments, register_deprecated_callbacks) -> function
            | { name; register_deprecated_argument = true; arg_t } as cb ->
                let arg =
                  ( name,
                    nullable_t (fun_t arg_t unit_t),
                    Some Lang.null,
                    Some
                      (Printf.sprintf
                         "This argument is deprecated! Please use the `%s` \
                          source method."
                         name) )
                in
                let { value = register_callback } = callback cb in
                let register_deprecated_callback s p =
                  match Lang.to_option (List.assoc name p) with
                    | None -> ()
                    | Some fn ->
                        (s#log : Log.t)#severe
                          "The `%s` argument is deprecated! Please use the \
                           source's `%s` method."
                          name name;
                        let register = register_callback s in
                        ignore
                          (Lang.apply register
                             [("synchronous", Lang.bool true); ("", fn)])
                in
                ( arg :: arguments,
                  register_deprecated_callback :: register_deprecated_callbacks
                )
            | _ -> (arguments, register_deprecated_callbacks))
          ([], []) callbacks

let add_operator ~(category : Doc.Value.source) ~descr ?(flags = [])
    ?(meth = ([] : ((< Source.source ; .. > as 'a) -> value) meth list))
    ?(callbacks = ([] : 'a callback list)) ?base name arguments ~return_t f =
  let compare (x, _, _, _) (y, _, _, _) =
    match (x, y) with
      | "", "" -> 0
      | _, "" -> -1
      | "", _ -> 1
      | x, y -> Stdlib.compare x y
  in
  let callback_arguments, register_deprecated_callbacks =
    deprecated_callback_registration_arguments callbacks
  in
  let arguments =
    ( "id",
      nullable_t string_t,
      Some null,
      Some "Force the value of the source ID." )
    :: List.stable_sort compare (arguments @ callback_arguments)
  in
  let meth =
    if category = `Output then
      meth
      @ [
          {
            name = "shutdown";
            scheme = ([], fun_t [] unit_t);
            descr = "Shutdown the output.";
            value =
              (fun s ->
                val_fun [] (fun _ ->
                    Clock.detach s#clock (s :> Clock.source);
                    s#sleep (s :> Clock.source);
                    unit));
          };
        ]
    else meth
  in
  let meth =
    List.map (fun m -> (m, `Method)) meth
    @ List.map (fun c -> (callback c, `Callback)) callbacks
  in
  let f env =
    let return_t, env = check_arguments ~return_t ~env arguments in
    let src : < Source.source ; .. > = f env in
    List.iter (fun register -> register src env) register_deprecated_callbacks;
    src#set_stack (Liquidsoap_lang.Lang_core.pos env);
    Typing.(src#frame_type <: return_t);
    ignore
      (Option.map
         (fun id -> src#set_id id)
         (to_valued_option to_string (List.assoc "id" env)));
    let v =
      let src = (src :> Source.source) in
      if category = `Output then source_methods ~base:unit src else source src
    in
    _meth v (List.map (fun ({ name; value }, _) -> (name, value src)) meth)
  in
  let base_t =
    if category = `Output then unit_t else source_t ~methods:false return_t
  in
  let return_t = source_methods_t base_t in
  let return_t = _method_t return_t meth in
  let category = `Source category in
  add_builtin ~category ~descr ~flags ?base name arguments return_t f

let add_track_operator ~(category : Doc.Value.source) ~descr ?(flags = [])
    ?(meth = ([] : ('a -> value) meth list)) ?base name arguments ~return_t f =
  let arguments =
    ( "id",
      nullable_t string_t,
      Some null,
      Some "Force the value of the track ID." )
    :: arguments
  in
  let f env =
    let return_t, env = check_arguments ~return_t ~env arguments in
    let field, (src : < Source.source ; .. >) = f env in
    src#set_stack (Liquidsoap_lang.Lang_core.pos env);
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
    _meth v (List.map (fun { name; value } -> (name, value src)) meth)
  in
  let return_t =
    method_t return_t
      (List.map (fun { name; scheme; descr } -> (name, scheme, descr)) meth)
  in
  let category = `Track category in
  add_builtin ~category ~descr ~flags ?base name arguments return_t f

let iter_sources ?(on_imprecise = fun () -> ()) f v =
  let rec iter_term env v =
    let iter_base_term env v =
      match v.Term.term with
        | `Cache_env _ | `Int _ | `Float _ | `Bool _ | `String _ | `Custom _
        | `Encoder _ ->
            ()
        | `List l -> List.iter (iter_term env) l
        | `Tuple l -> List.iter (iter_term env) l
        | `Null -> ()
        | `Hide (a, _) -> iter_term env a
        | `Cast { Term.cast = a } -> iter_term env a
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
              iter_value v
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
    if not (has_value_flag v Flags.itered_value) then (
      add_value_flag v Flags.itered_value;
      Value.Methods.iter (fun _ v -> iter_value v) (Value.methods v);
      match v with
        | _ when Source_val.is_value v -> f (Source_val.of_value v)
        | Value.Int _ | Value.String _ | Value.Float _ | Value.Bool _
        | Value.Custom _ ->
            ()
        | Value.List { value = l } -> List.iter iter_value l
        | Value.Tuple { value = l } -> List.iter iter_value l
        | Value.Null _ -> ()
        | Value.Fun { fun_args = proto; fun_env = env; fun_body = body } ->
            (* The following is necessarily imprecise: we might see sources that
               will be unused in the execution of the function. *)
            iter_term env body;
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
        | Value.FFI { ffi_args = proto; _ } ->
            on_imprecise ();
            List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto)
  in
  iter_value v

let iter_sources = iter_sources
