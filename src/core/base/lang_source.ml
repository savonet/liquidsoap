(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
module Runtime = Liquidsoap_lang.Runtime
open Lang

let eval ?toplevel ?typecheck ?cache ?deprecated ?ty ?name ~stdlib s =
  try eval ?toplevel ?typecheck ?cache ?deprecated ?ty ?name ~stdlib s
  with exn -> (
    let bt = Printexc.get_raw_backtrace () in
    match exn with
      | Runtime_error.Runtime_error _ -> Printexc.raise_with_backtrace exn bt
      | _ ->
          (Liquidsoap_lang.Runtime.throw ~lexbuf:None ~bt ()) exn;
          Printexc.raise_with_backtrace Runtime.Error bt)

let apply ?pos v env =
  try
    let ret = apply ?pos v env in
    Clock.after_eval ();
    ret
  with exn -> (
    let bt = Printexc.get_raw_backtrace () in
    match exn with
      | Runtime_error.Runtime_error _ -> Printexc.raise_with_backtrace exn bt
      | _ ->
          (Runtime.throw ~lexbuf:None ~bt ()) exn;
          Printexc.raise_with_backtrace Runtime.Error bt)

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
  list
    (List.map
       (fun (k, v) ->
         let v = string v in
         let k = string k in
         product k v)
       m)

let metadata m = metadata_list (Frame.Metadata.to_list m)
let metadata_track_t = Format_type.metadata
let track_marks_t = Format_type.track_marks

module Activation_val = Liquidsoap_lang.Lang_core.MkCustom (struct
  type content = Clock.activation

  let name = "activation"
  let to_string a = Printf.sprintf "<activation(id=%s>" a#id

  let to_json ~pos _ =
    Runtime_error.raise ~pos
      ~message:(Printf.sprintf "Activations cannot be represented as json")
      "json"

  let compare a1 a2 = Stdlib.compare a1#id a2#id
end)

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

(** Method description for source methods. The [scheme] field is a function of
    the source's frame type so that composition methods can wire their source
    type arguments to the operator's own frame type, avoiding global type
    variable pollution across call sites. Non-composition methods ignore the
    argument. *)
type source_meth = {
  name : string;
  scheme : Type.t -> Type.scheme;
  descr : string;
  value : Source.source -> value;
}

type 'a callback = {
  name : string;
  params : callback_param list;
  descr : string;
  register_deprecated_argument : bool;
  arg_t : (bool * string * t) list;
  register : params:(string * value) list -> 'a -> (env -> unit) -> unit;
}

let callback { name; params; descr; arg_t; register } : _ Lang.meth =
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
          let on_frame _ = on_frame [] in
          let before = Lang.to_bool (List.assoc "before" p) in
          if before then s#on_frame (`Before_frame on_frame)
          else s#on_frame (`After_frame (fun _ -> on_frame ())));
    };
    {
      name = "on_frame_checksum";
      params =
        [
          {
            name = "before";
            typ =
              Lang.(
                nullable_t (fun_t [(false, "", nullable_t string_t)] unit_t));
            default = Some Lang.null;
          };
        ];
      descr =
        "Register callbacks to compute frame checksums for debugging purposes. \
         This is useful to track frame content changes through the streaming \
         pipeline. The `before` callback is called before computing the frame \
         with the checksum of the cached frame (if any, `null` otherwise). The \
         main callback is called after computing the frame with the checksum \
         of the generated frame and the remaining cache (if any).";
      register_deprecated_argument = false;
      arg_t = [(false, "cache", nullable_t string_t); (false, "", string_t)];
      register =
        (fun ~params:p s after_cb ->
          let before_cb = Lang.to_option (List.assoc "before" p) in
          Option.iter
            (fun cb ->
              s#on_frame
                (`Before_frame
                   (fun cache ->
                     let checksum =
                       match cache with
                         | Some frame -> Lang.string (Frame.checksum frame)
                         | None -> Lang.null
                     in
                     ignore (apply cb [("", checksum)]))))
            before_cb;
          s#on_frame
            (`After_frame
               (fun { Source.frame; cache } ->
                 let frame_checksum = Lang.string (Frame.checksum frame) in
                 let cache_checksum =
                   match cache with
                     | Some c -> Lang.string (Frame.checksum c)
                     | None -> Lang.null
                 in
                 after_cb [("cache", cache_checksum); ("", frame_checksum)])));
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

let source_methods : source_meth list =
  [
    {
      name = "id";
      scheme = (fun _ -> ([], fun_t [] string_t));
      descr = "Identifier of the source.";
      value = (fun s -> val_fun [] (fun _ -> string s#id));
    };
    {
      name = "is_ready";
      scheme = (fun _ -> ([], fun_t [] bool_t));
      descr =
        "Indicate if a source is ready to stream. This does not mean that the \
         source is currently streaming, just that its resources are all \
         properly initialized.";
      value = (fun s -> val_fun [] (fun _ -> bool s#is_ready));
    };
    {
      name = "generate_frame";
      scheme = (fun _ -> ([], fun_t [] unit_t));
      descr =
        "Generate a frame from the source without consuming it. This can be \
         useful in advanced cases where generating a frame is required to \
         trigger some side effect like calculating some metadata before making \
         a decision. You should make sure that the source is available before \
         calling this function and it should only be called inside synchronous \
         streaming loop callback such as `on_frame`!";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              ignore s#peek_frame;
              unit));
    };
    {
      name = "insert_metadata";
      scheme =
        (fun _ ->
          ( [],
            Lang.fun_t
              [(true, "new_track", Lang.bool_t); (false, "", metadata_t)]
              Lang.unit_t ));
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
      scheme = (fun _ -> ([], ref_t bool_t));
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
      scheme = (fun _ -> ([], fun_t [] (list_t (product_t string_t float_t))));
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
      scheme = (fun _ -> ([], fun_t [] (nullable_t metadata_t)));
      descr = "Return the last metadata from the source.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              match s#last_metadata with
                | None -> null
                | Some (_, m) -> metadata m));
    };
    {
      name = "clear_last_metadata";
      scheme = (fun _ -> ([], fun_t [] unit_t));
      descr = "Clear the last metadata from the source.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              s#clear_last_metadata;
              Lang.unit));
    };
    {
      name = "register_command";
      scheme =
        (fun _ ->
          ( [],
            fun_t
              [
                (true, "usage", Lang.nullable_t Lang.string_t);
                (false, "description", Lang.string_t);
                (false, "", Lang.string_t);
                ( false,
                  "",
                  Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t );
              ]
              unit_t ));
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
      scheme = (fun _ -> ([], fun_t [] float_t));
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
      scheme = (fun _ -> ([], fun_t [] float_t));
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
      scheme = (fun _ -> ([], fun_t [] float_t));
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
      scheme = (fun _ -> ([], fun_t [] bool_t));
      descr = "Is the source currently controlling its own real-time loop.";
      value = (fun s -> val_fun [] (fun _ -> bool (snd s#self_sync <> None)));
    };
    {
      name = "self_sync_description";
      scheme = (fun _ -> ([], fun_t [] string_t));
      descr = "";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              match s#self_sync with
                | `Static, Some src ->
                    string ("Static: " ^ Clock_base.string_of_sync_source src)
                | `Dynamic, Some src ->
                    string
                      ("Dynamic synchronization source. Current one: "
                      ^ Clock_base.string_of_sync_source src)
                | `Dynamic, None ->
                    string "Dynamic synchronization source. Current one: none"
                | `Static, None -> string ""));
    };
    {
      name = "log";
      scheme = (fun _ -> ([], record_t [("level", ref_t int_t)]));
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
      scheme = (fun _ -> ([], fun_t [] bool_t));
      descr =
        "Indicate that the source can be asked to produce some data at any \
         time. This is `true` when the source is currently being used or if it \
         could be used at any time, typically inside a `switch` or `fallback`.";
      value = (fun s -> val_fun [] (fun _ -> bool s#is_up));
    };
    {
      name = "is_active";
      scheme = (fun _ -> ([], fun_t [] bool_t));
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
      scheme = (fun _ -> ([], fun_t [(false, "", float_t)] float_t));
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
      scheme = (fun _ -> ([], fun_t [] unit_t));
      descr = "Skip to the next track.";
      value =
        (fun s ->
          val_fun [] (fun _ ->
              s#abort_track;
              unit));
    };
    {
      name = "fallible";
      scheme = (fun _ -> ([], bool_t));
      descr = "Indicate if a source may fail, i.e. may not be ready to stream.";
      value = (fun s -> bool s#fallible);
    };
    {
      name = "clock";
      scheme = (fun _ -> ([], Lang_clock.ClockValue.base_t));
      descr = "The source's clock";
      value = (fun s -> Lang_clock.ClockValue.to_base_value s#clock);
    };
    {
      name = "time";
      scheme = (fun _ -> ([], fun_t [] float_t));
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

(** Default composition profile values for file and live sources. These are
    mutable so that stdlib.liq can update the live defaults (e.g. to install a
    fade-based on_select) after all operators load. *)

type composition_profile = {
  on_leave : value;
  on_select : value;
  track_sensitive : bool;
  replay_metadata : bool;
}

let noop_on_leave =
  eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_) -> ()"

let passthrough_on_select =
  eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (x) -> x.starting"

let file_profile =
  ref
    {
      on_leave = noop_on_leave;
      on_select = passthrough_on_select;
      track_sensitive = true;
      replay_metadata = true;
    }

let live_profile =
  ref
    {
      on_leave = noop_on_leave;
      on_select = passthrough_on_select;
      track_sensitive = false;
      replay_metadata = true;
    }

let profile_of s =
  if s#resolved_composition = `File then !file_profile else !live_profile

let profile_to_value p =
  record
    [
      ("on_leave", p.on_leave);
      ("on_select", p.on_select);
      ("track_sensitive", bool p.track_sensitive);
      ("replay_metadata", bool p.replay_metadata);
    ]

let profile_of_value v =
  {
    on_leave = Value.invoke v "on_leave";
    on_select = Value.invoke v "on_select";
    track_sensitive = to_bool (Value.invoke v "track_sensitive");
    replay_metadata = to_bool (Value.invoke v "replay_metadata");
  }

let source_composition_methods : source_meth list =
  [
    {
      name = "track_sensitive";
      scheme = (fun _ -> ([], getter_t bool_t));
      descr =
        "Whether this source is track-sensitive by default in switch \
         operators. File-based sources default to `true`; live network and \
         hardware inputs default to `false`. The value is looked up at runtime \
         from the active composition profile (`source.composition.file` or \
         `source.composition.live` depending on `composition_type`).";
      value =
        (fun s -> val_fun [] (fun _ -> bool (profile_of s).track_sensitive));
    };
    {
      name = "single";
      scheme = (fun _ -> ([], bool_t));
      descr =
        "Forbid the selection of this source for two consecutive tracks in \
         switch operators. Defaults to `false`.";
      value = (fun _ -> bool false);
    };
    {
      name = "replay_metadata";
      scheme = (fun _ -> ([], bool_t));
      descr =
        "Whether to replay the latest metadata on this source when it is \
         selected in a switch operator. Defaults to `true`.";
      value = (fun s -> bool (profile_of s).replay_metadata);
    };
    {
      name = "on_leave";
      scheme =
        (fun source_t ->
          let arg_t =
            record_t [("source", source_t); ("track_sensitive", bool_t)]
          in
          ([], fun_t [(false, "", arg_t)] unit_t));
      descr =
        "Called after switching away from this source in a switch operator, \
         once the transition using the ending source has completed. Receives a \
         record with `source` (the source being left) and `track_sensitive` \
         (`true` if the ending source's last frame had a track mark, i.e. it \
         finished naturally; `false` if it was preempted mid-track). This \
         function must return quickly as it runs in the streaming thread. The \
         default depends on the active composition profile: file sources \
         (`source.composition.file`) skip the source when preempted mid-track \
         so it starts fresh on its next selection, and do nothing when it \
         finished naturally; live sources (`source.composition.live`) do \
         nothing.";
      value =
        (fun s ->
          val_fun
            [("", "", None)]
            (fun p ->
              let x = List.assoc "" p in
              apply (profile_of s).on_leave [("", x)]));
    };
    {
      name = "on_select";
      scheme =
        (fun source_t ->
          let arg_t =
            record_t
              [
                ("ending", nullable_t source_t);
                ("replay_metadata", bool_t);
                ("starting", source_t);
              ]
          in
          ([], fun_t [(false, "", arg_t)] source_t));
      descr =
        "Called when selecting this source in a switch operator. Receives a \
         record with `starting` (the incoming source) and `ending` (the \
         previous source, or `null` when the ending source reached a track \
         boundary). Returns the source to use. By default, both file and live \
         profiles fade out `ending` when non-null (up to 1 second). The \
         default is looked up at runtime from the active composition profile \
         (`source.composition.file` or `source.composition.live` depending on \
         `composition_type`).";
      value =
        (fun s ->
          val_fun
            [("", "", None)]
            (fun p ->
              let x = List.assoc "" p in
              apply (profile_of s).on_select [("", x)]));
    };
  ]

let non_composition_source_methods =
  List.map (fun m -> (m, `Method)) source_methods
  @ List.map
      (fun c ->
        let { Lang.name; scheme; descr; value } = callback c in
        ({ name; scheme = (fun _ -> scheme); descr; value }, `Callback))
      source_callbacks

let source_methods =
  non_composition_source_methods
  @ List.map (fun m -> (m, `Composition)) source_composition_methods

(** Returns the composition tag to set and an optional description for the
    composition_type method. Outputs get neither; live inputs get `Live; passive
    inputs get `File; all other operators are passthrough and inherit from their
    effective source. *)
let composition_setup_of_category = function
  | `Output -> (None, None)
  | `Input `Active ->
      (Some `Live, Some "This source uses live composition by default.")
  | `Input `Passive | `Synthesis ->
      (Some `File, Some "This source uses file composition by default.")
  | _ ->
      (None, Some "This source inherits composition from its effective source.")

(** Set [src]'s composition tag based on [category] and return the
    [(name, value)] pair for the [composition_type] method, or [] for outputs.
*)
let setup_composition ~category src =
  let tag_opt, descr_opt = composition_setup_of_category category in
  Option.iter src#set_composition tag_opt;
  match descr_opt with
    | None -> []
    | Some _ ->
        let src = (src :> Source.source) in
        [
          ( "composition_type",
            reference
              (fun () ->
                string
                  (if src#resolved_composition = `File then "file" else "live"))
              (fun v ->
                src#set_composition
                  (match to_string v with
                    | "file" -> `File
                    | "live" -> `Live
                    | s ->
                        Runtime_error.raise ~pos:[]
                          ~message:
                            (Printf.sprintf "Invalid composition type: %S" s)
                          "invalid")) );
        ]

(** Register source.composition.{file,live}.{on_leave,on_select,track_sensitive}
    as mutable Liquidsoap references. Must be called after the [source] module
    exists in the environment.  Pass the string returned by
    [Lang.add_operator "source" ...] as [~base]. *)
let register_composition_module ~base () =
  let composition = add_module ~base "composition" in
  let frame_t = Type.var () in
  let source_frame_t =
    Type.make
      (Type.Constr
         { Type.constructor = "source"; params = [(`Invariant, frame_t)] })
  in
  let on_leave_t =
    fun_t
      [
        ( false,
          "",
          record_t [("source", source_frame_t); ("track_sensitive", bool_t)] );
      ]
      unit_t
  in
  let on_select_t =
    fun_t
      [
        ( false,
          "",
          record_t
            [
              ("ending", nullable_t source_frame_t);
              ("replay_metadata", Lang.bool_t);
              ("starting", source_frame_t);
            ] );
      ]
      source_frame_t
  in
  let profile_t =
    record_t
      [
        ("on_leave", on_leave_t);
        ("on_select", on_select_t);
        ("track_sensitive", bool_t);
        ("replay_metadata", bool_t);
      ]
  in
  List.iter
    (fun (name, profile_ref) ->
      ignore
        (add_builtin_value ~base:composition ~category:`Liquidsoap
           ~descr:
             (Printf.sprintf
                "Set the default composition profile for %s sources. The \
                 profile record contains `on_leave`, `on_select`, \
                 `track_sensitive`, and `replay_metadata` defaults used by \
                 switch operators."
                name)
           name
           (val_fun
              [("", "", None)]
              (fun p ->
                profile_ref := profile_of_value (List.assoc "" p);
                unit))
           (fun_t [(false, "", profile_t)] unit_t)))
    [("file", file_profile); ("live", live_profile)]

let make_t ?pos = Type.make ?pos:(Option.map Pos.of_lexing_pos pos)

let _method_t t l =
  List.fold_left
    (fun t ({ Lang.name; scheme; descr = doc }, category) ->
      Type.meth ~doc ~category name scheme t)
    t l

let _source_method_t t l =
  let source_t = t in
  List.fold_left
    (fun t ({ name; scheme; descr = doc }, category) ->
      Type.meth ~doc ~category name (scheme source_t) t)
    t l

let source_methods_t t = _source_method_t t source_methods

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

(** Like [source_methods ~base:unit] but omits composition methods (for outputs,
    which cannot participate in composition). *)
let output_source_value s =
  meth unit
    (List.map
       (fun ({ name; value }, _) -> (name, value s))
       non_composition_source_methods)

let output_source_methods_t t =
  _source_method_t t non_composition_source_methods

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

let is_nullable t =
  match Type.deref t with Type.{ descr = Nullable _ } -> true | _ -> false

(** Ensure that the frame contents of all the sources occurring in the value
    agree with [t]. *)
let check_content v t =
  let check t t' = Typing.(t <: t') in
  let rec check_value v t =
    if not (has_value_flag v Flags.checked_value) then (
      add_value_flag v Flags.checked_value;
      match (v, (Type.deref t).Type.descr) with
        | _ when Source_val.is_value v ->
            let source_t = source_t (Source_val.of_value v)#frame_type in
            check source_t t
        | _ when Track.is_value v ->
            let field, s = Track.of_value v in
            if
              field <> Frame.Fields.track_marks
              && field <> Frame.Fields.metadata
            then (
              let typ =
                Frame_type.make (Type.var ())
                  (Frame.Fields.add field t Frame.Fields.empty)
              in
              try check s#frame_type typ with _ when is_nullable t -> ())
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
        | _, Type.Var _ -> ()
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
                let { Lang.value = register_callback } = callback cb in
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
    ?(callbacks = ([] : 'a callback list)) ?self_sync_description ?base name
    arguments ~return_t f =
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
                    unit));
          };
        ]
    else meth
  in
  let meth =
    match self_sync_description with
      | None -> meth
      | Some sync_descr ->
          meth
          @ [
              {
                name = "self_sync_description";
                scheme = ([], fun_t [] string_t);
                descr = sync_descr;
                value =
                  (fun s ->
                    val_fun [] (fun _ ->
                        match s#self_sync with
                          | `Static, Some src ->
                              string
                                ("Static: "
                                ^ Clock_base.string_of_sync_source src)
                          | `Dynamic, Some src ->
                              string
                                ("Dynamic synchronization source. Current one: "
                                ^ Clock_base.string_of_sync_source src)
                          | `Dynamic, None ->
                              string
                                "Dynamic synchronization source. Current one: \
                                 none"
                          | `Static, None -> string ""));
              };
            ]
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
    let composition_meths = setup_composition ~category src in
    Typing.(src#frame_type <: return_t);
    ignore
      (Option.map
         (fun id -> src#set_id id)
         (to_valued_option to_string (List.assoc "id" env)));
    let v =
      let src = (src :> Source.source) in
      if category = `Output then output_source_value src else source src
    in
    let v =
      _meth v
        (List.map (fun ({ Lang.name; value }, _) -> (name, value src)) meth)
    in
    _meth v composition_meths
  in
  let base_t =
    if category = `Output then unit_t else source_t ~methods:false return_t
  in
  let return_t =
    (if category = `Output then output_source_methods_t else source_methods_t)
      base_t
  in
  let composition_scheme_meths =
    match snd (composition_setup_of_category category) with
      | None -> []
      | Some descr -> [("composition_type", ([], ref_t string_t), descr)]
  in
  let return_t =
    List.fold_left
      (fun t (name, scheme, doc) ->
        Type.meth ~doc ~category:`Composition name scheme t)
      (_method_t return_t meth) composition_scheme_meths
  in
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
    _meth v (List.map (fun { Lang.name; value } -> (name, value src)) meth)
  in
  let return_t =
    method_t return_t
      (List.map
         (fun { Lang.name; scheme; descr } -> (name, scheme, descr))
         meth)
  in
  let category = `Track category in
  add_builtin ~category ~descr ~flags ?base name arguments return_t f
