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

(** Abstract operator which selects one of its children sources
  * either at the beginning of a track or at every frame,
  * depending on a parametrizable predicate.
  * A few specializations of it are defined below. *)

open Source

type child = {
  source_getter : Lang.value;
  mutable current_source : source option;
  (* A transition is a value of type (source,source) -> source *)
  transition : Lang.value;
}

(** The switch can either happen at any time in the stream (insensitive)
  * or only at track limits (sensitive). *)
type track_mode = Sensitive | Insensitive

type selection = {
  child : child;
  child_source : source;
  effective_source : source;
}

exception Reference

class virtual switch ~name ~override_meta ~transition_length
  ?(mode = fun () -> true) ?(replay_meta = true) ~static_sources
  (cases : child list) =
  let used_sources =
    List.fold_left
      (function
        | `Dynamic -> fun _ -> `Dynamic
        | `Static l ->
            fun { source_getter; current_source; transition } ->
              List.fold_left
                (function
                  | `Dynamic -> fun _ -> `Dynamic
                  | `Static l -> (
                      fun v ->
                        try
                          let sources = ref l in
                          Lang.iter_sources
                            ~on_reference:(fun () -> raise Reference)
                            (fun s -> sources := s :: !sources)
                            v;
                          `Static !sources
                        with Reference -> `Dynamic))
                (`Static
                  (l @ match current_source with None -> [] | Some s -> [s]))
                [source_getter; transition])
      (`Static []) cases
  in
  let self_sync_type =
    match used_sources with
      | `Dynamic -> lazy `Dynamic
      | `Static l -> Utils.self_sync_type l
  in
  object (self)
    inherit operator ~name static_sources
    val mutable transition_length = transition_length
    val mutable selected : selection option = None

    (** We have to explicitly manage our children as they are dynamically created
    * by application of the transition functions. In particular we need a list
    * of all children that have output data in the current round. *)
    val mutable to_finish = []

    (** Indicates that the former child was left without having finished its
    * track, in which case the switch will artificially produce an EOT. *)
    val mutable need_eot = false

    (** The selection method should return None or Some c,
    * where c is a ready child. *)
    method virtual private select : child option

    (** Don't call #select directly but use #cached_select
    * to ensure consistency during one time tick between #is_ready and
    * #get_frame. We want to make sure that when a source is selected
    * during a #is_ready call, the same selection is returned during the
    * next #get_frame call. *)
    val mutable cached_selected = None

    method private cached_select =
      match cached_selected with
        | Some _ as c -> c
        | None ->
            cached_selected <- self#select;
            cached_selected

    method! after_output =
      (* Advance the memo frame. *)
      self#advance;

      (* Propagate to all sub-sources, i.e. our children and the transitions
       * wrapping them:
       *  - current transition is [selected],
       *  - old one in [to_finish]. *)
      List.iter
        (function { current_source = Some s } -> s#after_output | _ -> ())
        cases;
      begin
        match selected with
          | None -> ()
          | Some s ->
              s.child_source#after_output;
              s.effective_source#after_output
      end;
      List.iter (fun s -> s#after_output) to_finish;
      to_finish <- [];

      (* Selection may have been triggered by a call to #is_ready, without
       * any call to #get_ready (in particular if #select returned None).
       * It is cleared here in order to get a chance to be re-computed later. *)
      cached_selected <- None

    val mutable activation = []

    method! private wake_up activator =
      activation <- (self :> source) :: activator;
      let wake_up =
        Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation)
      in
      List.iter
        (fun { transition; source_getter; _ } ->
          wake_up transition;
          wake_up source_getter)
        cases

    method! private sleep =
      let sleep =
        Lang.iter_sources (fun s -> s#leave ~dynamic:true (self :> source))
      in
      List.iter
        (fun { transition; source_getter; _ } ->
          sleep transition;
          sleep source_getter)
        cases;
      match selected with
        | None -> ()
        | Some s -> s.effective_source#leave (self :> source)

    method is_ready = need_eot || selected <> None || self#cached_select <> None

    (* This one is tricky. We do not want to call #cached_select as
       this requires some run-time info from underlying sources
       (mostly ctype to be set). The only case that matters if no
       sources are selected is to know if we are [`Static, false] for
       any caller such as [cross]. Since the source is not ready, we
       can return anything so we check if any source might be not
       self_sync in this case *)
    method self_sync =
      ( Lazy.force self_sync_type,
        match selected with
          | Some s -> snd s.effective_source#self_sync
          | None ->
              List.exists
                (function
                  | { current_source = Some s } -> snd s#self_sync | _ -> false)
                cases )

    method private get_frame ab =
      (* Choose the next child to be played.
       * [forget] tells that the current child has finished its track,
       * in which case a transition does not make sense and would actually start
       * playing a next track (if available) on the left child. *)
      let reselect ?(forget = false) () =
        if not forget then need_eot <- true;
        match
          let c = self#cached_select in
          cached_selected <- None;
          c
        with
          (* We assume that the source is always computed during the #select function call. *)
          | Some { current_source = None } -> assert false
          | Some ({ current_source = Some s } as c) -> (
              match selected with
                | None ->
                    self#log#important "Switch to %s." s#id;
                    let new_source =
                      (* Force insertion of old metadata if relevant.
                       * It can't be done in a static way: we need to start
                       * pulling data to see if new metadata comes out, in case
                       * the source was shared and kept streaming from somewhere
                       * else (this is thanks to Frame.get_chunk).
                       * A quicker hack might have been doable if there wasn't a
                       * transition in between. *)
                      match s#last_metadata with
                        | Some m when replay_meta ->
                            new Insert_metadata.replay m s
                        | _ -> s
                    in
                    new_source#get_ready activation;
                    selected <-
                      Some
                        {
                          child = c;
                          child_source = s;
                          effective_source = new_source;
                        }
                | Some ({ child_source = old_source } as old_selection)
                  when old_source != s ->
                    self#log#important "Switch to %s with%s transition." s#id
                      (if forget then " forgetful" else "");
                    old_selection.effective_source#leave (self :> source);
                    to_finish <- old_selection.effective_source :: to_finish;
                    Clock.collect_after (fun () ->
                        let old_source =
                          if forget then Debug_sources.empty ()
                          else old_selection.child_source
                        in
                        let new_source =
                          (* Force insertion of old metadata if relevant.
                           * It can't be done in a static way: we need to start
                           * pulling data to see if new metadata comes out, in case
                           * the source was shared and kept streaming from somewhere
                           * else (this is thanks to Frame.get_chunk).
                           * A quicker hack might have been doable if there wasn't a
                           * transition in between. *)
                          match s#last_metadata with
                            | Some m when replay_meta ->
                                new Insert_metadata.replay m s
                            | _ -> s
                        in
                        let s =
                          Lang.to_source
                            (Lang.apply c.transition
                               [
                                 ("", Lang.source old_source);
                                 ("", Lang.source new_source);
                               ])
                        in
                        let s =
                          match s#id with
                            | id when id = new_source#id -> s
                            | _ ->
                                let s =
                                  new Max_duration.max_duration
                                    ~override_meta ~duration:transition_length s
                                in
                                new Sequence.sequence
                                  ~merge:true [s; new_source]
                        in
                        Clock.unify s#clock self#clock;
                        s#get_ready activation;
                        selected <-
                          Some
                            {
                              child = c;
                              child_source = s;
                              effective_source = s;
                            })
                | _ ->
                    (* We are staying on the same child,
                     * don't start a new track. *)
                    need_eot <- false)
          | None -> (
              match selected with
                | Some old_s ->
                    old_s.effective_source#leave (self :> source);
                    to_finish <- old_s.effective_source :: to_finish;
                    selected <- None
                | None -> ())
      in
      (* #select is called only when selected=None, and the cache is cleared
       * as soon as the new selection is set. *)
      assert (selected = None || cached_selected = None);
      if need_eot then (
        need_eot <- false;
        Frame.add_break ab (Frame.position ab))
      else (
        match selected with
          | None ->
              reselect ~forget:true ();

              (* Our #is_ready, and caching, ensure the following. *)
              assert (selected <> None);
              self#get_frame ab
          | Some s ->
              s.effective_source#get ab;
              if Frame.is_partial ab then reselect ~forget:true ()
              else if not (mode ()) then reselect ())

    method remaining =
      match selected with None -> 0 | Some s -> s.effective_source#remaining

    method abort_track =
      match selected with
        | Some s -> s.effective_source#abort_track
        | None -> ()

    method seek n =
      match selected with Some s -> s.effective_source#seek n | None -> 0

    method selected = Option.map (fun { child_source } -> child_source) selected
  end

(** Common tools for Lang bindings of switch operators *)

let default_transition =
  Lang.val_fun [("", "x", None); ("", "y", None)] (fun e -> List.assoc "y" e)

(** Switch: switch according to user-defined predicates. *)

let satisfied f = Lang.to_bool (Lang.apply f [])

let trivially_true = function
  | {
      Lang.value =
        Lang.Fun (_, _, { Term.term = Term.(Ground (Ground.Bool true)); _ });
      _;
    } ->
      true
  | _ -> false

let third (_, _, s) = s

(** Like [List.find] but evaluates [f] on every element when [strict] is
    [true]. *)
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

class lang_switch ~override_meta ~all_predicates ~transition_length mode
  ?replay_meta (children : (Lang.value * bool * child) list) =
  let static_sources =
    List.fold_left
      (fun static_sources -> function
        | _, _, ({ source_getter } as child)
          when not (Lang.is_function source_getter) ->
            let s = Lang.to_source source_getter in
            child.current_source <- Some (Lang.to_source source_getter);
            s :: static_sources
        | _ -> static_sources)
      [] children
  in
  object (self)
    inherit
      switch
        ~name:"switch" ~mode ~override_meta ~transition_length ?replay_meta
          ~static_sources (List.map third children) as super

    method private select =
      let selected c =
        match selected with
          | Some { child } when child == c -> true
          | _ -> false
      in
      (* Returns a source if predicate is true. *)
      let prepare (d, child) =
        let satisfied = satisfied d in
        match (satisfied, child.current_source) with
          | false, _ -> None
          | true, None ->
              let s =
                Lang.to_source ((Lang.to_getter child.source_getter) ())
              in
              s#get_ready [(self :> Source.source)];
              child.current_source <- Some s;
              Some s
          | true, Some s -> Some s
      in
      try
        Some
          (third
             (find ~strict:all_predicates
                (fun (d, single, c) ->
                  match prepare (d, c) with
                    | Some s -> (not (single && selected c)) && s#is_ready
                    | None -> false)
                children))
      with Not_found -> None

    method stype =
      if
        List.exists
          (function
            | d, single, { source_getter; current_source = Some s } ->
                trivially_true d && (not single)
                && (not (Lang.is_function source_getter))
                && s#stype = `Infallible
            | _ -> false)
          children
      then `Infallible
      else `Fallible

    method! after_output =
      List.iter
        (function
          | d, _, ({ source_getter; current_source = Some s } as child)
            when Lang.is_function source_getter && not (satisfied d) ->
              s#leave (self :> Source.source);
              child.current_source <- None
          | _ -> ())
        children;
      super#after_output
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let pred_t = Lang.fun_t [] Lang.bool_t in
  Lang.add_operator "switch" ~category:`Track
    ~descr:
      "At the beginning of a track, select the first source whose predicate is \
       true."
    ~meth:
      [
        ( "selected",
          ([], Lang.fun_t [] (Lang.nullable_t Lang.(source_t return_t))),
          "Currently selected source.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#selected with
                  | Some s -> Lang.source s
                  | None -> Lang.null) );
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
        Lang.list_t
          (Lang.product_t pred_t (Lang.getter_t (Lang.source_t return_t))),
        None,
        Some "Sources with the predicate telling when they can be played." );
    ]
    ~return_t
    (fun p ->
      let children =
        List.map
          (fun p ->
            let pred, s = Lang.to_product p in
            (pred, s))
          (Lang.to_list (List.assoc "" p))
      in
      let ts = Lang.to_bool_getter (List.assoc "track_sensitive" p) in
      let tr =
        let l = List.length children in
        let tr = Lang.to_list (List.assoc "transitions" p) in
        let ltr = List.length tr in
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
          (fun transition (f, source_getter) ->
            (f, { source_getter; current_source = None; transition }))
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
      new lang_switch
        ~replay_meta ~override_meta ~all_predicates ~transition_length:tl ts
        children)
