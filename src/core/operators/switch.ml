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
  object (self)
    inherit operator ~name [src] as super
    val mutable first = true

    (* [last_metadata] and [clear_last_metadata] are delegated to [src] but
       [set_last_metadata] is not: the frame machinery's reset-on-track would
       clear the child's last metadata and only re-set our own, erasing the
       metadata needed when replaying it on re-selection. *)
    initializer self#set_reset_last_metadata_on_track false
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method effective_source = src#effective_source
    method! last_metadata = src#last_metadata
    method! clear_last_metadata = src#clear_last_metadata

    (* Our own caching is disabled in favor of [src]'s: we are thrown away on
       every selection change, so anything cached here would be dropped along
       with us. *)
    method! consumed n = src#consumed n

    method private generate_frame =
      let buf = src#peek_frame in
      super#consumed (Frame.position buf);
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
  mutable effective_track_sensitive : bool option;
  mutable effective_predicate : bool option;
}

type selection = {
  child : child;
  (* What we actually stream: whatever [on_select] returned. *)
  effective_source : source;
  (* [child.source] wrapped so that its first frame carries a track mark. *)
  proxy : source;
  sleep : unit -> unit;
}

(** A source we have switched away from. A transition may keep pulling from it
    for a while, so its [on_leave] only runs once nothing holds it any more. *)
type leaving = { leaving_proxy : source; fire : unit -> unit }

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
  let self_sync_type = Clock.self_sync_type_of_sources sources in
  let track_sensitive = Atomic.make true in
  object (self)
    inherit operator ~name:"switch" sources as super

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive:(fun () -> Atomic.get track_sensitive)
        ()

    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected

    (* Sources we have switched away from, waiting to be released. *)
    val mutable leaving : leaving list = []

    (* [track_sensitive] tells the source how it ended: [true] when it had
       nothing left for its current track, [false] when we cut into a track it
       was still playing. This is the switch's own decision, not something the
       source reports: a source that simply ran out of data was not preempted,
       even though it never got to emit a track mark. *)
    method private push_leaving ~track_sensitive s =
      leaving <-
        {
          leaving_proxy = s.proxy;
          fire = (fun () -> s.child.on_leave s.proxy track_sensitive);
        }
        :: leaving

    method private release_leaving ~force () =
      leaving <-
        List.filter
          (fun l ->
            if force || not l.leaving_proxy#is_up then (
              l.fire ();
              false)
            else true)
          leaving

    method exchange_selected v =
      Option.iter
        (fun old_selection -> old_selection.sleep ())
        (Atomic.exchange selected v);
      if Option.is_none v then self#release_leaving ~force:true ();
      self#notify_sync_source (snd self#self_sync)

    initializer self#on_sleep (fun () -> self#exchange_selected None)

    (* We cannot reselect the same source twice during a streaming cycle. *)
    val mutable excluded_sources = []

    (* A selection only holds while we are being animated: when we resume after
       going quiet it has to be re-evaluated, and nothing is playing for a new
       one to interrupt. A parent that is not playing us does not animate us at
       all, which is what we detect here. Being animated without being pulled is
       not quiet: a passive clock is ticked by its parent on cycles where its
       consumer asks for no data. *)
    val mutable last_animated_tick = -1
    val mutable resuming = false

    initializer
      self#on_before_streaming_cycle (fun () ->
          let tick = Clock.ticks self#clock in
          resuming <- 1 < tick - last_animated_tick;
          last_animated_tick <- tick;
          excluded_sources <- [];
          Atomic.set track_sensitive (List.for_all is_track_sensitive children));
      self#on_after_streaming_cycle (fun () ->
          List.iter
            (fun c ->
              c.effective_track_sensitive <- None;
              c.effective_predicate <- None)
            children);
      self#on_frame
        (`After_frame (fun _ -> self#release_leaving ~force:false ()))

    (* We are at a track boundary when the selected source has no more data for
       its current track: either it could not fill the frame past the position
       we asked for, or it is not ready anymore. Anywhere else, the selected
       source is still playing a track and taking over means preempting it. *)
    method private at_boundary ~reselect =
      match (reselect, self#selected) with
        | `After_position _, _ -> true
        | _, None -> true
        | _, Some s -> resuming || not s.effective_source#is_ready

    method private select ~reselect ~boundary () =
      let may_select c =
        match self#selected with
          | Some { child; effective_source } when child.source == c.source ->
              (not c.single) && self#can_reselect ~reselect effective_source
          | Some { child; _ } ->
              (* Cutting into a track that is still playing requires that at
                 least one of the two sources involved does not insist on
                 track boundaries: the one being left must accept being
                 interrupted, or the one starting must not need to start on a
                 boundary. *)
              (boundary
              || (not (is_track_sensitive child))
              || not (is_track_sensitive c))
              && not (List.memq c excluded_sources)
          | None -> not (List.memq c excluded_sources)
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

    (* [leaving] is the selection we are switching away from, if any, and always
       gets its [on_leave] called. [ending] is the source [on_select] gets to
       transition out of: only a source that is still playing a track has
       something left to blend. *)
    method private apply_on_select ~leaving:previous ~ending child =
      let starting =
        new insert_initial_track_mark
          ~name:(Printf.sprintf "%s.proxy" child.source#id)
          child.source
      in
      Typing.(starting#frame_type <: self#frame_type);

      (* [on_select] runs on every selection, so what it registers on the
         sources it can reach — the ones we hand it, and our own children, which
         it may well have captured — would otherwise pile up on them for good.
         They only need to be around for as long as this selection. *)
      let { Lang_source.release = release_callbacks; result = effective_source }
          =
        Lang_source.collect_callback_releases
          (sources @ ((starting :> Source.source) :: Option.to_list ending))
          (fun () -> child.on_select ending starting)
      in
      Typing.(effective_source#frame_type <: self#frame_type);

      (* Wake the new graph before putting the previous selection to sleep: when
         it holds [previous.proxy] this keeps that source continuously up. *)
      let a = effective_source#wake_up (self :> Clock.source) in
      let sleep () =
        effective_source#sleep a;
        release_callbacks ()
      in

      Option.iter
        (self#push_leaving ~track_sensitive:(Option.is_none ending))
        previous;

      self#exchange_selected
        (Some { child; effective_source; proxy = starting; sleep })

    (* A track-sensitive child keeps its slot until its track ends, even if its
       predicate has gone false in the meantime. *)
    method private still_wanted c = is_track_sensitive c || is_ready c

    (* A transition is in progress while something we switched away from is
       still being pulled from. *)
    method private transition_in_progress =
      List.exists (fun l -> l.leaving_proxy#is_up) leaving

    (* Whether to stick with the current selection rather than re-evaluate. *)
    method private keep_selected ~reselect s =
      if self#transition_in_progress then (
        (* A transition is in progress: let it play out, but never hand back a
           graph that has no data left past the position we were asked for. *)
          match reselect with
          | `After_position _ -> self#can_reselect ~reselect s.effective_source
          | `Ok | `Force -> true)
      else (
        (* Outside a transition, any track boundary forces a re-evaluation. *)
          match reselect with
          | `After_position _ | `Force -> false
          | `Ok -> s.effective_source#is_ready)

    method get_source ~reselect () =
      match self#selected with
        | Some s
          when (not resuming) && self#still_wanted s.child
               && self#keep_selected ~reselect s ->
            Some s.effective_source
        | _ -> (
            let boundary = self#at_boundary ~reselect in
            begin match
              ( self#selected,
                self#select
                (* If we've returned the same source, it should be accepted now. *)
                  ~reselect:(match reselect with `Force -> `Ok | v -> v)
                  ~boundary () )
            with
              | None, None -> ()
              | Some old_selection, None ->
                  (* Nothing to switch to: the source is not being cut into,
                     it simply has nothing left. *)
                  self#push_leaving ~track_sensitive:true old_selection;
                  self#exchange_selected None
              | None, Some c ->
                  self#log#important "Switch to %s." c.source#id;
                  self#apply_on_select ~leaving:None ~ending:None c
              | Some old_selection, Some c
                when old_selection.child.source == c.source ->
                  ()
              | Some old_selection, Some c ->
                  self#log#important "Switch to %s %s." c.source#id
                    (if boundary then "at track boundary" else "with transition");
                  (* Only a source that is still playing a track has something
                     left to transition out of. *)
                  let ending =
                    if boundary then None else Some old_selection.proxy
                  in
                  self#apply_on_select ~leaving:(Some old_selection) ~ending c
            end;
            match self#selected with
              | Some s when s.effective_source#is_ready ->
                  excluded_sources <- s.child :: excluded_sources;
                  Some s.effective_source
              | _ -> None)

    method self_sync =
      ( self_sync_type (),
        match self#selected with
          | Some s -> snd s.effective_source#self_sync
          | None -> None )

    method remaining =
      match self#selected with
        | None -> 0
        | Some s -> s.effective_source#remaining

    (* The metadata of a track we drop goes with it: it describes a track that
       will never resume, and replaying it on re-selection would announce it a
       second time. [clear_last_metadata] alone does not reach here, the frame
       machinery calls it on every track mark. *)
    method abort_track =
      match self#selected with
        | Some s ->
            s.effective_source#clear_last_metadata;
            s.effective_source#abort_track
        | None -> ()

    (* With no selection this is [self], and [resolved_composition] then falls
       back to the children, which is what we want. *)
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
    Type.meth ~optional:true "replay_metadata" ([], Lang.getter_t Lang.bool_t)
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
            (* The composition methods are declared optional, so fall back to
               the active profile when a value was built without them. Every
               source built by [Lang.add_operator] does carry them, and their
               own defaults resolve the profile the same way. Resolution is
               deferred so that all of them follow [composition_type]. *)
            let profile () = Lang_source.profile_of source in
            let track_sensitive =
              match find_opt "track_sensitive" s_val with
                | Some v -> Lang.to_bool_getter v
                | None -> fun () -> (profile ()).track_sensitive
            in
            let replay_metadata =
              match find_opt "replay_metadata" s_val with
                | Some v -> Lang.to_bool_getter v
                | None -> fun () -> (profile ()).replay_metadata
            in
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
                | None -> fun s ts -> call (profile ()).on_leave s ts
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
                      ("replay_metadata", Lang.bool (replay_metadata ()));
                      ("starting", Lang.source starting);
                    ]
                in
                Lang.to_source (Lang.apply on_select [("", record)])
              in
              match find_opt "on_select" s_val with
                | Some on_select -> call on_select
                | None ->
                    fun ending starting ->
                      call (profile ()).on_select ending starting
            in
            {
              predicate = pred;
              source;
              on_select;
              on_leave;
              track_sensitive;
              single;
              effective_track_sensitive = None;
              effective_predicate = None;
            })
          (Lang.to_list (List.assoc "" p))
      in
      let all_predicates = Lang.to_bool (List.assoc "all_predicates" p) in
      new switch ~all_predicates children)
