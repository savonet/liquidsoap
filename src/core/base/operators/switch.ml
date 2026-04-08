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

(** Custom operator which selects one of its children sources either at the
    beginning of a track or at every frame, depending on a parametrizable
    predicate. A few specializations of it are defined below. *)

open Source

class insert_initial_track_mark ~name src =
  object
    inherit operator ~name [src]
    val mutable first = true
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method effective_source = src#effective_source
    method! last_metadata = src#last_metadata

    method private generate_frame =
      let buf = src#get_frame in
      if first then (
        first <- false;
        if not (Frame.has_track_marks buf) then Frame.add_track_mark buf 0
        else buf)
      else buf
  end

type child = {
  predicate : Lang.value;
  source : source;
  on_select : source option -> source -> source;
  on_leave : source -> bool -> unit;
  track_sensitive : unit -> bool;
  single : bool;
  replay_metadata : bool;
  mutable effective_track_sensitive : bool option;
  mutable effective_predicate : bool option;
}

(** The switch can either happen at any time in the stream (insensitive) or only
    at track limits (sensitive). *)
type track_mode = Sensitive | Insensitive

type selection = {
  child : child;
  effective_source : source;
  proxy : source;
  sleep : unit -> unit;
  has_track_marks : bool ref;
  mutable pending_on_leave : (force:bool -> bool) option;
}

let is_ready c =
  match c.effective_predicate with
    | Some v -> v
    | None ->
        let v = Lang.to_bool (Lang.apply c.predicate []) in
        c.effective_predicate <- Some v;
        v

let is_track_sensitive c =
  match c.effective_track_sensitive with
    | Some v -> v
    | None ->
        let v = c.track_sensitive () in
        c.effective_track_sensitive <- Some v;
        v

let trivially_true = function
  | Value.Fun { fun_body = { Term.term = `Bool true } } -> true
  | _ -> false

(** Like [List.find] but evaluates [f] on every element when [strict] is [true].
*)
let find ?(strict = false) f l =
  let rec aux = function
    | x :: l ->
        if f x then (
          if strict then List.iter (fun x -> ignore (f x)) l;
          x)
        else aux l
    | [] -> raise Not_found
  in
  aux l

class switch ~all_predicates children =
  let sources = List.map (fun c -> c.source) children in
  let self_sync_type = Clock_base.self_sync_type sources in
  let track_sensitive = Atomic.make true in
  object (self)
    inherit operator ~name:"switch" sources

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive:(fun () -> Atomic.get track_sensitive)
        ()

    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected

    method exchange_selected v =
      Option.iter
        (fun old_selection ->
          old_selection.sleep ();
          if v = None then
            Option.iter
              (fun on_leave -> ignore (on_leave ~force:true))
              old_selection.pending_on_leave)
        (Atomic.exchange selected v)

    initializer
      self#on_sleep (fun () ->
          match Atomic.exchange selected None with
            | Some { sleep } -> sleep ()
            | _ -> ())

    (* We cannot reselect the same source twice during a streaming cycle. *)
    val mutable excluded_sources = []

    initializer
      self#on_before_streaming_cycle (fun () ->
          excluded_sources <- [];
          Atomic.set track_sensitive (List.for_all is_track_sensitive children));
      self#on_after_streaming_cycle (fun () ->
          List.iter
            (fun c ->
              c.effective_track_sensitive <- None;
              c.effective_predicate <- None)
            children);
      self#on_frame
        (`After_frame
           (fun _ ->
             match self#selected with
               | Some ({ pending_on_leave = Some on_leave; _ } as sel)
                 when on_leave ~force:false ->
                   sel.pending_on_leave <- None
               | _ -> ()))

    method private select ~reselect () =
      let may_select c =
        match self#selected with
          | Some { child; effective_source } when child.source == c.source ->
              (not c.single) && self#can_reselect ~reselect effective_source
          | _ -> not (List.memq c excluded_sources)
      in
      try
        Some
          (find ~strict:all_predicates
             (fun c -> is_ready c && may_select c && c.source#is_ready)
             children)
      with Not_found -> None

    method fallible =
      not
        (List.exists
           (fun c ->
             (not c.source#fallible) && (not c.single)
             && trivially_true c.predicate)
           children)

    method private prepare_ending =
      function
      | None -> (None, None)
      | Some s ->
          let proxy = s.proxy in
          let has_track_marks = s.has_track_marks in
          let on_leave = s.child.on_leave in
          let on_leave ~force =
            if force || not proxy#is_up then (
              on_leave proxy !has_track_marks;
              true)
            else false
          in
          (Some on_leave, Some s.proxy)

    method private apply_on_select ~ending child =
      let pending_on_leave, ending_source = self#prepare_ending ending in

      let starting =
        new insert_initial_track_mark
          ~name:(Printf.sprintf "%s.proxy" child.source#id)
          child.source
      in
      Typing.(starting#frame_type <: self#frame_type);

      let effective_source = child.on_select ending_source starting in
      Typing.(effective_source#frame_type <: self#frame_type);
      let a = effective_source#wake_up (self :> Clock.source) in
      let sleep () = effective_source#sleep a in

      let has_track_marks = ref false in
      starting#on_frame (`Before_frame (fun _ -> has_track_marks := false));
      starting#on_frame (`Track (fun _ -> has_track_marks := true));

      let selection =
        {
          child;
          effective_source;
          proxy = starting;
          sleep;
          has_track_marks;
          pending_on_leave;
        }
      in

      self#exchange_selected (Some selection)

    method get_source ~reselect () =
      match self#selected with
        | Some s
          when (is_track_sensitive s.child || is_ready s.child)
               (* We want to force a re-select on each new track unless there's a transition still in progress. *)
               && (s.pending_on_leave <> None
                  || self#can_reselect
                       ~reselect:
                         (* We want to force a re-select on each new track. *)
                           (match reselect with
                           | `After_position _ -> `Force
                           | v -> v)
                       s.effective_source) ->
            Some s.effective_source
        | _ -> (
            begin match
              ( self#selected,
                self#select
                (* If we've returned the same source, it should be accepted now. *)
                  ~reselect:(match reselect with `Force -> `Ok | v -> v)
                  () )
            with
              | None, None -> ()
              | Some _, None -> self#exchange_selected None
              | None, Some c ->
                  self#log#important "Switch to %s." c.source#id;
                  self#apply_on_select ~ending:None c
              | Some old_selection, Some c
                when old_selection.child.source == c.source ->
                  ()
              | Some old_selection, Some c ->
                  let track_sensitive = Atomic.get track_sensitive in
                  self#log#important "Switch to %s with %stransition."
                    c.source#id
                    (if track_sensitive then "track-sensitive " else "");
                  let ending =
                    if track_sensitive then None else Some old_selection
                  in
                  self#apply_on_select ~ending c
            end;
            match self#selected with
              | Some s when s.effective_source#is_ready ->
                  excluded_sources <- s.child :: excluded_sources;
                  Some s.effective_source
              | _ -> None)

    method self_sync =
      ( Lazy.force self_sync_type,
        match self#selected with
          | Some s -> snd s.effective_source#self_sync
          | None -> None )

    method remaining =
      match self#selected with
        | None -> 0
        | Some s -> s.effective_source#remaining

    method abort_track =
      match self#selected with
        | Some s -> s.effective_source#abort_track
        | None -> ()

    method effective_source =
      match self#selected with
        | Some s -> s.effective_source#effective_source
        | None -> (self :> Source.source)
  end

(** Common tools for Lang bindings of switch operators *)

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let pred_t = Lang.fun_t [] Lang.bool_t in
  (* Source type with optional per-source composition methods. Declaring them as
     optional allows passing plain sources as well as sources with explicit
     method overrides (e.g. s.{on_select = my_fn}). *)
  let source_t =
    let src_t = Lang.source_t return_t in
    let on_select_t =
      Lang.fun_t
        [
          ( false,
            "",
            Lang.record_t
              [
                ("ending", Lang.nullable_t (Lang.source_t return_t));
                ("replay_metadata", Lang.bool_t);
                ("starting", Lang.source_t return_t);
              ] );
        ]
        (Lang.source_t return_t)
    in
    let on_leave_t =
      Lang.fun_t
        [
          ( false,
            "",
            Lang.record_t
              [
                ("source", Lang.source_t return_t);
                ("track_sensitive", Lang.bool_t);
              ] );
        ]
        Lang.unit_t
    in
    Type.meth ~optional:true "replay_metadata" ([], Lang.bool_t)
    @@ Type.meth ~optional:true "single" ([], Lang.bool_t)
    @@ Type.meth ~optional:true "track_sensitive" ([], Lang.getter_t Lang.bool_t)
    @@ Type.meth ~optional:true "on_leave" ([], on_leave_t)
    @@ Type.meth ~optional:true "on_select" ([], on_select_t)
    @@ src_t
  in
  Lang.add_operator "switch" ~category:`Track
    ~descr:
      "At the beginning of a track, select the first source whose predicate is \
       true."
    ~meth:
      [
        {
          name = "selected";
          scheme = ([], Lang.fun_t [] (Lang.nullable_t Lang.(source_t return_t)));
          descr = "Currently selected source.";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ ->
                  match
                    Option.map (fun { child } -> child.source) s#selected
                  with
                    | Some s -> Lang.source s
                    | None -> Lang.null));
        };
      ]
    [
      ( "all_predicates",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Always evaluate all predicates when re-selecting." );
      ( "",
        Lang.list_t (Lang.product_t pred_t source_t),
        None,
        Some "Sources with the predicate telling when they can be played." );
    ]
    ~return_t
    (fun p ->
      let find_opt name s_val =
        Value.Methods.find_opt name (Value.methods s_val)
      in
      let children =
        List.map
          (fun p ->
            let pred, s_val = Lang.to_product p in
            let source = Lang.to_source s_val in
            let single =
              match find_opt "single" s_val with
                | Some v -> Lang.to_bool v
                | None -> false
            in
            let track_sensitive =
              match find_opt "track_sensitive" s_val with
                | Some v -> Lang.to_bool_getter v
                | None ->
                    let ts = source#composition = `File in
                    fun () -> ts
            in
            let profile = Lang_source.profile_of source in
            let on_leave =
              let call on_leave s ts =
                let record =
                  Lang.record
                    [
                      ("source", Lang.source s);
                      ("track_sensitive", Lang.bool ts);
                    ]
                in
                ignore (Lang.apply on_leave [("", record)])
              in
              match find_opt "on_leave" s_val with
                | Some on_leave -> call on_leave
                | None -> call profile.on_leave
            in
            let replay_metadata =
              match find_opt "replay_metadata" s_val with
                | Some v -> Lang.to_bool v
                | None -> profile.replay_metadata
            in
            let on_select =
              let call on_select ending starting =
                let record =
                  Lang.record
                    [
                      ( "ending",
                        match ending with
                          | None -> Lang.null
                          | Some s -> Lang.source s );
                      ("replay_metadata", Lang.bool replay_metadata);
                      ("starting", Lang.source starting);
                    ]
                in
                Lang.to_source (Lang.apply on_select [("", record)])
              in
              match find_opt "on_select" s_val with
                | Some on_select -> call on_select
                | None -> call profile.on_select
            in
            {
              predicate = pred;
              source;
              on_select;
              on_leave;
              track_sensitive;
              single;
              replay_metadata;
              effective_track_sensitive = None;
              effective_predicate = None;
            })
          (Lang.to_list (List.assoc "" p))
      in
      let all_predicates = Lang.to_bool (List.assoc "all_predicates" p) in
      new switch ~all_predicates children)
