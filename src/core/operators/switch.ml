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

    (* Once the switch has left us we only deliver the rest of the track that
       was playing: the child's next track is never heard through us. *)
    val mutable left = false
    val mutable track_ended = false
    method leave = left <- true
    method track_ended = track_ended

    (* [last_metadata] and [clear_last_metadata] are delegated to [src] but
       [set_last_metadata] is not: the frame machinery's reset-on-track would
       clear the child's last metadata and only re-set our own, erasing the
       metadata needed when replaying it on re-selection. *)
    initializer self#set_reset_last_metadata_on_track false
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready && not (left && track_ended)

    method abort_track =
      track_ended <- true;
      src#abort_track

    method remaining = src#remaining
    method private self_sync = src#cached_self_sync
    method effective_source = src#effective_source
    method! last_metadata = src#last_metadata
    method! clear_last_metadata = src#clear_last_metadata

    (* Our own caching is disabled in favor of [src]'s: we are thrown away on
       every selection change, so anything cached here would be dropped along
       with us. *)
    method! consumed n = src#consumed n

    method private generate_frame =
      let buf = src#peek_frame in
      let starts_track = Frame.track_marks buf = [0] in
      if left && starts_track && not first then (
        (* The child moved on to its next track without stating the end of
           the one we were left on. *)
        track_ended <- true;
        super#consumed 0;
        self#empty_frame)
      else (
        super#consumed (Frame.position buf);
        if src#segment_end = `Track_end then track_ended <- true;
        if first then (
          first <- false;
          if not (Frame.has_track_marks buf) then Frame.add_track_mark buf 0
          else buf)
        else buf)
  end

type child = {
  predicate : Lang.value;
  source : source;
  on_select : source option -> source -> source;
  on_leave : source -> bool -> unit;
  track_sensitive : unit -> bool;
  single : bool;
  effective_track_sensitive : bool Source.Per_cycle.t;
  effective_predicate : bool Source.Per_cycle.t;
}

type selection = {
  child : child;
  (* What we actually stream: whatever [on_select] returned. *)
  effective_source : source;
  (* [child.source] wrapped so that its first frame carries a track mark. *)
  proxy : insert_initial_track_mark;
  sleep : unit -> unit;
}

(** A child we have switched away from. A transition may keep pulling from it
    for a while, so its [on_leave] only runs once nothing holds it any more. *)
type leaving = {
  left : child;
  leaving_proxy : insert_initial_track_mark;
  preempted : bool;
}

(** What a switch would read from here. Deciding changes nothing: the handoff
    happens when the frame is read. *)
type decision =
  [ `Nothing
  | `Keep
  | `Handoff of child
  | `Preempt of child
  | (* Nothing else can play: the leaves are completed first. *)
    `Reselect of child ]

let trivially_true = function
  | Value.Fun { fun_body = { Term.term = `Bool true } } -> true
  | _ -> false

class switch children =
  let sources = List.map (fun c -> c.source) children in
  let self_sync_type = Source_sync.type_of_sources sources in
  object (self)
    inherit operator ~name:"switch" sources
    inherit generate_from_multiple_sources ()

    (* Every track of a child is a track of the switch. *)
    method private next_entry = `Track
    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected
    val mutable leaving : leaving list = []

    method private push_leaving ~preempted s =
      s.proxy#leave;
      leaving <-
        { left = s.child; leaving_proxy = s.proxy; preempted } :: leaving

    (* A child was preempted when the switch cut into its track and that track
       had not run out by the time nothing read it any more. *)
    method private fire_on_leave l =
      l.left.on_leave
        (l.leaving_proxy :> source)
        (l.preempted && not l.leaving_proxy#track_ended)

    method private release_leaving ~force () =
      leaving <-
        List.filter
          (fun l ->
            if force || not l.leaving_proxy#is_up then (
              self#fire_on_leave l;
              false)
            else true)
          leaving

    method private is_leaving c = List.exists (fun l -> l.left == c) leaving

    method exchange_selected v =
      Option.iter
        (fun old_selection -> old_selection.sleep ())
        (Atomic.exchange selected v);
      if Option.is_none v then self#release_leaving ~force:true ()

    initializer
      self#on_sleep (fun () -> self#exchange_selected None);
      self#on_frame
        (`After_frame (fun _ -> self#release_leaving ~force:false ()))

    method fallible =
      not
        (List.exists
           (fun c ->
             (not c.source#fallible) && (not c.single)
             && trivially_true c.predicate)
           children)

    val predicates_snapshot : unit Source.Per_cycle.t = Source.Per_cycle.make ()

    (* Predicates are evaluated only once a decision needs one, so keeping a
       track-sensitive selection costs no script call.

       They are then read as one snapshot, so a script writing several of them
       inside an atomic section is never observed part way through. *)
    method private predicate_holds c =
      self#per_cycle predicates_snapshot (fun () ->
          Atomic_section.run (fun () ->
              List.iter
                (fun c ->
                  ignore (self#evaluate_predicate c);
                  ignore (self#is_track_sensitive c))
                children));
      self#evaluate_predicate c

    method private evaluate_predicate c =
      self#per_cycle c.effective_predicate (fun () ->
          Lang.to_bool (Lang.apply c.predicate []))

    method private is_track_sensitive c =
      self#per_cycle c.effective_track_sensitive c.track_sensitive

    method private can_play c = self#predicate_holds c && c.source#is_ready

    (* While a track plays. Cutting into it requires that one of the two
       children involved does not insist on track boundaries. *)
    method private decide_mid_track s : decision =
      let candidate c =
        c != s.child
        && (not (self#is_leaving c))
        && not (self#is_track_sensitive s.child && self#is_track_sensitive c)
      in
      if self#is_track_sensitive s.child && not (List.exists candidate children)
      then `Keep
      else (
        match
          List.find_opt
            (fun c ->
              if c == s.child then self#predicate_holds c
              else candidate c && self#can_play c)
            children
        with
          | Some c when c == s.child -> `Keep
          | Some c -> `Preempt c
          | None -> if self#is_track_sensitive s.child then `Keep else `Nothing)

    (* Where the selection has nothing left for its track, or there is none. A
       [single] child does not follow itself. *)
    method private decide_at_boundary : decision =
      let follows_itself c =
        c.single
        && match self#selected with Some s -> s.child == c | None -> false
      in
      let first ~leaving =
        List.find_opt
          (fun c ->
            (leaving || not (self#is_leaving c))
            && (not (follows_itself c))
            && self#can_play c)
          children
      in
      match (first ~leaving:false, self#selected) with
        | Some c, Some s when c == s.child && s.effective_source#is_ready ->
            `Keep
        | Some c, _ -> `Handoff c
        | None, _ when leaving = [] -> `Nothing
        | None, _ -> (
            match first ~leaving:true with
              | Some c -> `Reselect c
              | None -> `Nothing)

    val mutable decision : decision = `Nothing

    method! private choose_next_source ~boundary =
      decision <-
        (match self#selected with
          | Some s when (not boundary) && s.effective_source#is_ready ->
              self#decide_mid_track s
          | _ -> self#decide_at_boundary);
      decision <> `Nothing

    (* [ending] is the source [on_select] gets to transition out of: only a
       track that was cut has something left to blend. The selection left
       always gets its [on_leave]. *)
    method private handoff ~preempted child =
      let previous = self#selected in
      let ending =
        if preempted then Option.map (fun s -> (s.proxy :> source)) previous
        else None
      in
      self#log#important "Switch to %s%s." child.source#id
        (match (previous, preempted) with
          | None, _ -> ""
          | Some _, true -> " with transition"
          | Some _, false -> " at track boundary");
      let starting =
        new insert_initial_track_mark
          ~name:(Printf.sprintf "%s.proxy" child.source#id)
          child.source
      in
      Typing.(starting#frame_type <: self#frame_type);

      (* A source with a content type escapes the runtime type checks of
         [on_select]'s body, whose static type may be narrower than ours. *)
      starting#content_type_computation_allowed;
      ignore starting#content_type;

      (* [on_select] runs on every selection, so what it registers on the
         sources it can reach — the ones we hand it, and our own children, which
         it may well have captured — would otherwise pile up on them for good.
         They only need to be around for as long as this selection. *)
      let { Lang_source.release = release_callbacks; result = effective_source }
          =
        Lang_source.collect_callback_releases
          (sources @ ((starting :> Source.source) :: Option.to_list ending))
          (fun () -> child.on_select ending (starting :> source))
      in
      Typing.(effective_source#frame_type <: self#frame_type);

      (* A source built by [on_select] sits in a clock of its own until it is
         made ours, and is never ready there. *)
      Clock.unify ~pos:self#pos effective_source#clock self#clock;

      (* Wake the new graph before putting the previous selection to sleep: when
         it holds [previous.proxy] this keeps that source continuously up. *)
      let a = effective_source#wake_up (self :> Clock.source) in
      let sleep () =
        effective_source#sleep a;
        release_callbacks ()
      in

      (* We answered ready because the child is: what [on_select] returned
         has to deliver now, or the child is played as it is. *)
      let effective_source, sleep =
        if effective_source#is_ready then (effective_source, sleep)
        else (
          self#log#important
            "The source selected for %s has nothing to deliver: playing %s \
             itself."
            child.source#id child.source#id;
          let a = starting#wake_up (self :> Clock.source) in
          sleep ();
          ((starting :> source), fun () -> starting#sleep a))
      in
      Option.iter (self#push_leaving ~preempted) previous;
      self#exchange_selected
        (Some { child; effective_source; proxy = starting; sleep })

    method! private enter_chosen_source =
      (match decision with
        | `Nothing | `Keep -> ()
        | `Handoff c -> self#handoff ~preempted:false c
        | `Preempt c -> self#handoff ~preempted:true c
        | `Reselect c ->
            self#release_leaving ~force:true ();
            self#handoff ~preempted:false c);
      decision <- `Keep;
      Option.map (fun s -> s.effective_source) self#selected

    method get_source ~boundary:_ () = self#enter_chosen_source

    method private self_sync =
      ( self_sync_type (),
        match self#selected with
          | Some s -> snd s.effective_source#cached_self_sync
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
      self#end_track;
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
              [("source", Lang.source_t return_t); ("preempted", Lang.bool_t)]
          );
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
              let call on_leave s preempted =
                let record =
                  Lang.record
                    [
                      ("source", Lang.source s);
                      ("preempted", Lang.bool preempted);
                    ]
                in
                ignore (Lang.apply on_leave [("", record)])
              in
              match find_opt "on_leave" s_val with
                | Some on_leave -> call on_leave
                | None ->
                    fun s preempted -> call (profile ()).on_leave s preempted
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
              effective_track_sensitive = Source.Per_cycle.make ();
              effective_predicate = Source.Per_cycle.make ();
            })
          (Lang.to_list (List.assoc "" p))
      in
      new switch children)
