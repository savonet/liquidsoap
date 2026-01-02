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

class insert_initial_track_mark src =
  object
    inherit operator ~name:"insert_initial_track_mark" [src]
    val mutable first = true
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method effective_source = src#effective_source

    method private generate_frame =
      let buf = src#get_frame in
      if first then (
        first <- false;
        Frame.add_track_mark buf 0)
      else buf
  end

(* A transition is a value of type (source,source) -> source *)
type transition = Lang.value
type child = { source : source; transition : transition }

(** The switch can either happen at any time in the stream (insensitive) or only
    at track limits (sensitive). *)
type track_mode = Sensitive | Insensitive

type selection = {
  predicate : Lang.value;
  child : child;
  effective_source : source;
}

let satisfied f = Lang.to_bool (Lang.apply f [])

let trivially_true = function
  | Value.Fun { fun_body = { Term.term = `Bool true } } -> true
  | _ -> false

let pick_selection (p, _, s) = (p, s)

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

class switch ~all_predicates ~override_meta ~transition_length ~replay_meta
  ~track_sensitive children =
  let cases = List.map (fun (_, _, s) -> s) children in
  let sources = List.map (fun c -> c.source) cases in
  let self_sync_type = Clock_base.self_sync_type sources in
  object (self)
    (* Don't pass sources to operator base class - they're managed through
       effective_source wrappers created by prepare_new_source. Passing them
       causes double wake_up: once from operator base, once from wrapper. *)
    inherit operator ~name:"switch" []

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive ()

    val mutable transition_length = transition_length
    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected

    method set_selected v =
      (match v with
        | Some { child; effective_source } when effective_source != child.source
          ->
            effective_source#wake_up (self :> Clock.activation)
        | _ -> ());
      match Atomic.exchange selected v with
        | Some { child; effective_source } when effective_source != child.source
          ->
            effective_source#sleep (self :> Clock.activation)
        | _ -> ()

    initializer
      self#on_sleep (fun () ->
          match Atomic.exchange selected None with
            | Some { child; effective_source }
              when effective_source != child.source ->
                effective_source#sleep (self :> Clock.activation)
            | _ -> ())

    (* We cannot reselect the same source twice during a streaming cycle. *)
    val mutable excluded_sources = []

    initializer
      (* Manually unify clocks since we don't pass sources to operator base *)
      List.iter
        (fun s -> Clock.unify ~pos:self#pos self#clock s.source#clock)
        cases;
      self#on_before_streaming_cycle (fun () -> excluded_sources <- [])

    method private select ~reselect () =
      let may_select ~single s =
        match self#selected with
          | Some { child; effective_source } when child.source == s.source ->
              (not single) && self#can_reselect ~reselect effective_source
          | _ -> not (List.memq s excluded_sources)
      in
      try
        Some
          (pick_selection
             (find ~strict:all_predicates
                (fun (d, single, s) ->
                  satisfied d && may_select ~single s && s.source#is_ready)
                children))
      with Not_found -> None

    method fallible =
      not
        (List.exists
           (fun (d, single, s) ->
             (not s.source#fallible) && (not single) && trivially_true d)
           children)

    method private prepare_new_source source =
      let new_source = new insert_initial_track_mark source in
      Typing.(new_source#frame_type <: self#frame_type);
      match (source#last_metadata, replay_meta) with
        | Some (_, m), true ->
            let new_source = new Replay_metadata.replay m new_source in
            Typing.(new_source#frame_type <: self#frame_type);
            new_source
        | _ -> new_source

    method get_source ~reselect () =
      match self#selected with
        | Some s
          when (track_sensitive () || satisfied s.predicate)
               && self#can_reselect
                    ~reselect:
                      (* We want to force a re-select on each new track. *)
                        (match reselect with
                        | `After_position _ -> `Force
                        | v -> v)
                    s.effective_source ->
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
              | Some _, None -> self#set_selected None
              | None, Some (predicate, c) ->
                  self#log#important "Switch to %s." c.source#id;
                  let new_source = self#prepare_new_source c.source in
                  self#set_selected
                    (Some
                       { predicate; child = c; effective_source = new_source })
              | Some old_selection, Some (_, c)
                when old_selection.child.source == c.source ->
                  ()
              | old_selection, Some (predicate, c) ->
                  let forget, old_source =
                    match old_selection with
                      | None -> (true, Debug_sources.empty ())
                      | Some old_selection -> (false, old_selection.child.source)
                  in
                  self#log#important "Switch to %s with%s transition."
                    c.source#id
                    (if forget then " forgetful" else "");
                  let new_source = self#prepare_new_source c.source in
                  let s =
                    Lang.to_source
                      (Lang.apply c.transition
                         [
                           ("", Lang.source old_source);
                           ("", Lang.source new_source);
                         ])
                  in
                  let s =
                    if s == new_source then s
                    else (
                      Typing.(s#frame_type <: self#frame_type);
                      let s =
                        new Max_duration.max_duration
                          ~override_meta ~duration:transition_length s
                      in
                      Typing.(s#frame_type <: self#frame_type);
                      let s =
                        new Sequence.sequence ~merge:true [s; new_source]
                      in
                      Typing.(s#frame_type <: self#frame_type);
                      (s :> Source.source))
                  in
                  self#set_selected
                    (Some { predicate; child = c; effective_source = s })
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

let default_transition =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_, y) -> y"

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let pred_t = Lang.fun_t [] Lang.bool_t in
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
      ( "track_sensitive",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some "Re-select only on end of tracks." );
      ( "transition_length",
        Lang.float_t,
        Some (Lang.float 5.),
        Some "Maximum transition duration." );
      ( "override",
        Lang.string_t,
        Some (Lang.string "liq_transition_length"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the `transition_length` parameter." );
      ( "replay_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Replay the last metadata of a child when switching to it in the \
           middle of a track." );
      ( "all_predicates",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Always evaluate all predicates when re-selecting." );
      (let transition_t =
         Lang.fun_t
           [
             (false, "", Lang.source_t return_t);
             (false, "", Lang.source_t return_t);
           ]
           (Lang.source_t return_t)
       in
       ( "transitions",
         Lang.list_t transition_t,
         Some (Lang.list []),
         Some "Transition functions, padded with `fun (x,y) -> y` functions." ));
      ( "single",
        Lang.list_t Lang.bool_t,
        Some (Lang.list []),
        Some
          "Forbid the selection of a branch for two tracks in a row. The empty \
           list stands for `[false,...,false]`." );
      ( "",
        Lang.list_t (Lang.product_t pred_t (Lang.source_t return_t)),
        None,
        Some "Sources with the predicate telling when they can be played." );
    ]
    ~return_t
    (fun p ->
      let children =
        List.map
          (fun p ->
            let pred, s = Lang.to_product p in
            (pred, Lang.to_source s))
          (Lang.to_list (List.assoc "" p))
      in
      let ts = Lang.to_bool_getter (List.assoc "track_sensitive" p) in
      let tr = Lang.to_list (List.assoc "transitions" p) in
      let ltr = List.length tr in
      let tr =
        let l = List.length children in
        if ltr > l then
          raise
            (Error.Invalid_value
               (List.assoc "transitions" p, "Too many transitions"));
        if ltr < l then tr @ List.init (l - ltr) (fun _ -> default_transition)
        else tr
      in
      let replay_meta = Lang.to_bool (List.assoc "replay_metadata" p) in
      let tl =
        Frame.main_of_seconds (Lang.to_float (List.assoc "transition_length" p))
      in
      let override_meta = Lang.to_string (List.assoc "override" p) in
      let all_predicates = Lang.to_bool (List.assoc "all_predicates" p) in
      let singles =
        List.map Lang.to_bool (Lang.to_list (List.assoc "single" p))
      in
      let singles =
        if singles = [] then List.init (List.length children) (fun _ -> false)
        else singles
      in
      let children =
        List.map2
          (fun t (f, s) -> (f, { source = s; transition = t }))
          tr children
      in
      let children =
        try List.map2 (fun (d, s) single -> (d, single, s)) children singles
        with Invalid_argument s when s = "List.map2" ->
          raise
            (Error.Invalid_value
               ( List.assoc "single" p,
                 "there should be exactly one flag per children" ))
      in
      new switch
        ~replay_meta ~override_meta ~all_predicates ~transition_length:tl
        ~track_sensitive:ts children)
