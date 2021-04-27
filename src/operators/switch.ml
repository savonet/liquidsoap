(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(* A transition is a value of type (source,source) -> source *)
type transition = Lang.value

type child = {
  source : source;
  transition : transition;
  (* Also remind the current metadata in order to be able to restore it
   * when switching back in the middle of a track. This is an expected
   * behaviour, but notice that it makes extra assumptions about the nature of
   * metadata, tying them to the notion of track. *)
  mutable cur_meta : Request.metadata option;
}

(** The switch can either happen at any time in the stream (insensitive)
  * or only at track limits (sensitive). *)
type track_mode = Sensitive | Insensitive

class virtual switch ~kind ~name ~override_meta ~transition_length
  ?(mode = fun () -> true) ?(replay_meta = true) (cases : child list) =
  object (self)
    inherit operator ~name kind (List.map (fun x -> x.source) cases)

    val mutable transition_length = transition_length

    val mutable selected : (child * source) option = None

    (** We have to explictly manage our children as they are dynamically created
    * by application of the transition functions. In particular we need a list
    * of all children that have output data in the current round. *)
    val mutable to_finish = []

    (** Indicates that the former child was left without having finished its
    * track, in which case the switch will artifically produce an EOT. *)
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

    method after_output =
      (* Advance the memo frame. *)
      self#advance;

      (* Propagate to all sub-sources, i.e. our children and the transitions
       * wrapping them:
       *  - current transition is [selected],
       *  - old one in [to_finish]. *)
      List.iter (fun s -> s.source#after_output) cases;
      begin
        match selected with
        | None -> ()
        | Some (_, s) -> s#after_output
      end;
      List.iter (fun s -> s#after_output) to_finish;
      to_finish <- [];

      (* Selection may have been triggered by a call to #is_ready, without
       * any call to #get_ready (in particular if #select returned None).
       * It is cleared here in order to get a chance to be re-computed later. *)
      cached_selected <- None

    val mutable activation = []

    method private wake_up activator =
      activation <- (self :> source) :: activator;
      List.iter
        (fun { transition; source = s; _ } ->
          s#get_ready ~dynamic:true activation;
          Lang.iter_sources
            (fun s -> s#get_ready ~dynamic:true activation)
            transition)
        cases

    method private sleep =
      List.iter
        (fun { transition; source = s; _ } ->
          s#leave ~dynamic:true (self :> source);
          Lang.iter_sources
            (fun s -> s#leave ~dynamic:true (self :> source))
            transition)
        cases;
      match selected with None -> () | Some (_, s) -> s#leave (self :> source)

    method is_ready = need_eot || selected <> None || self#cached_select <> None

    method self_sync =
      match selected with
        | Some (_, source) -> source#self_sync
        | None -> (
            match self#cached_select with
              | Some { source } -> source#self_sync
              | None -> false )

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
          | Some c -> (
              match selected with
                | None ->
                    self#log#important "Switch to %s." c.source#id;

                    (* The source is already ready, this call is only there for
                     * allowing an uniform treatment of switches, triggering
                     * a #leave call. *)
                    c.source#get_ready activation;
                    selected <- Some (c, c.source)
                | Some (old_c, old_s) when old_c != c ->
                    self#log#important "Switch to %s with%s transition."
                      c.source#id
                      (if forget then " forgetful" else "");
                    old_s#leave (self :> source);
                    to_finish <- old_s :: to_finish;
                    Clock.collect_after (fun () ->
                        let old_source =
                          if forget then Blank.empty kind else old_c.source
                        in
                        let new_source =
                          (* Force insertion of old metadata if relevant.
                           * It can't be done in a static way: we need to start
                           * pulling data to see if new metadata comes out, in case
                           * the source was shared and kept streaming from somewhere
                           * else (this is thanks to Frame.get_chunk).
                           * A quicker hack might have been doable if there wasn't a
                           * transition in between. *)
                          match c.cur_meta with
                            | Some m when replay_meta ->
                                new Insert_metadata.replay ~kind m c.source
                            | _ -> c.source
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
                          new Max_duration.max_duration
                            ~kind ~override_meta ~duration:transition_length s
                        in
                        let s =
                          new Sequence.sequence
                            ~kind ~merge:true [s; new_source]
                        in
                        Clock.unify s#clock self#clock;
                        s#get_ready activation;
                        selected <- Some (c, s))
                | _ ->
                    (* We are staying on the same child,
                     * don't start a new track. *)
                    need_eot <- false )
          | None -> (
              match selected with
                | Some (_, old_s) ->
                    old_s#leave (self :> source);
                    to_finish <- old_s :: to_finish;
                    selected <- None
                | None -> () )
      in
      (* #select is called only when selected=None, and the cache is cleared	
       * as soon as the new selection is set. *)
      assert (selected = None || cached_selected = None);
      if need_eot then (
        need_eot <- false;
        Frame.add_break ab (Frame.position ab) )
      else (
        match selected with
          | None ->
              reselect ~forget:true ();

              (* Our #is_ready, and caching, ensure the following. *)
              assert (selected <> None);
              self#get_frame ab
          | Some (c, s) ->
              s#get ab;
              c.cur_meta <-
                ( if Frame.is_partial ab then None
                else (
                  match
                    List.fold_left
                      (function
                        | None -> fun (p, m) -> Some (p, m)
                        | Some (curp, curm) ->
                            fun (p, m) ->
                              Some (if p >= curp then (p, m) else (curp, curm)))
                      ( match c.cur_meta with
                        | None -> None
                        | Some m -> Some (-1, m) )
                      (Frame.get_all_metadata ab)
                  with
                    | None -> None
                    | Some (_, m) -> Some (Hashtbl.copy m) ) );
              if Frame.is_partial ab then reselect ~forget:true ()
              else if not (mode ()) then reselect () )

    method remaining =
      match selected with None -> 0 | Some (_, s) -> s#remaining

    method abort_track =
      match selected with Some (_, s) -> s#abort_track | None -> ()

    method seek n = match selected with Some (_, s) -> s#seek n | None -> 0
  end

(** Common tools for Lang bindings of switch operators *)

let default_transition =
  Lang.val_fun [("", "x", None); ("", "y", None)] (fun e -> List.assoc "y" e)

(** Switch: switch according to user-defined predicates. *)

let satisfied f = Lang.to_bool (Lang.apply f [])

let trivially_true = function
  | {
      Lang.value =
        Lang.Fun
          ( _,
            _,
            _,
            { Lang_values.term = Lang_values.(Ground (Ground.Bool true)); _ } );
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
          x )
        else aux l
    | [] -> raise Not_found
  in
  aux l

class lang_switch ~kind ~override_meta ~all_predicates ~transition_length mode
  ?replay_meta (children : (Lang.value * bool * child) list) =
  object
    inherit
      switch
        ~name:"switch" ~kind ~mode ~override_meta ~transition_length
          ?replay_meta (List.map third children)

    method private select =
      let selected s =
        match selected with
          | Some (child, _) when child == s -> true
          | _ -> false
      in
      try
        Some
          (third
             (find ~strict:all_predicates
                (fun (d, single, s) ->
                  (* Check single constraints *)
                  (if selected s then not single else true)
                  && satisfied d && s.source#is_ready)
                children))
      with Not_found -> None

    method stype =
      if
        List.exists
          (fun (d, single, s) ->
            s.source#stype = Infallible && (not single) && trivially_true d)
          children
      then Infallible
      else Fallible
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  let pred_t = Lang.fun_t [] Lang.bool_t in
  Lang.add_operator "switch" ~category:Lang.TrackProcessing
    ~descr:
      "At the beginning of a track, select the first source whose predicate is \
       true."
    [
      ( "track_sensitive",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some "Re-select only on end of tracks." );
      ( "transition_length",
        Lang.float_t,
        Some (Lang.float 5.),
        Some "Maximun transition duration." );
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
      let tr =
        let l = List.length children in
        let tr = Lang.to_list (List.assoc "transitions" p) in
        let ltr = List.length tr in
        if ltr > l then
          raise
            (Lang_errors.Invalid_value
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
          (fun t (f, s) -> (f, { source = s; cur_meta = None; transition = t }))
          tr children
      in
      let children =
        try List.map2 (fun (d, s) single -> (d, single, s)) children singles
        with Invalid_argument s when s = "List.map2" ->
          raise
            (Lang_errors.Invalid_value
               ( List.assoc "single" p,
                 "there should be exactly one flag per children" ))
      in
      let kind = Source.Kind.of_kind kind in
      new lang_switch
        ~kind ~replay_meta ~override_meta ~all_predicates ~transition_length:tl
        ts children)
