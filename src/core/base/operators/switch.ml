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

type child = {
  source : source;
  on_select : transition;
  on_leave : Lang.value;
  replay_meta : bool;
  single : bool;
}

(** The switch can either happen at any time in the stream (insensitive) or only
    at track limits (sensitive). *)
type track_mode = Sensitive | Insensitive

type selection = {
  predicate : Lang.value;
  child : child;
  effective_source : source;
  sleep : unit -> unit;
}

let satisfied f = Lang.to_bool (Lang.apply f [])

let trivially_true = function
  | Value.Fun { fun_body = { Term.term = `Bool true } } -> true
  | _ -> false

let pick_selection (p, s) = (p, s)

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

class switch ~all_predicates ~track_sensitive children =
  let cases = List.map (fun (_, s) -> s) children in
  let sources = List.map (fun c -> c.source) cases in
  let self_sync_type = Clock_base.self_sync_type sources in
  object (self)
    inherit operator ~name:"switch" (List.map (fun x -> x.source) cases)

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive ()

    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected

    method exchange_selected v =
      match Atomic.exchange selected v with
        | Some { sleep } -> sleep ()
        | _ -> ()

    method set_selected ~predicate ~child effective_source =
      let sleep =
        if effective_source != child.source then (
          let a = effective_source#wake_up (self :> Clock.source) in
          fun () -> effective_source#sleep a)
        else fun () -> ()
      in
      self#exchange_selected
        (Some { predicate; child; effective_source; sleep })

    initializer
      self#on_sleep (fun () ->
          match Atomic.exchange selected None with
            | Some { sleep } -> sleep ()
            | _ -> ())

    (* We cannot reselect the same source twice during a streaming cycle. *)
    val mutable excluded_sources = []

    initializer
      self#on_before_streaming_cycle (fun () -> excluded_sources <- [])

    method private select ~reselect () =
      let may_select s =
        match self#selected with
          | Some { child; effective_source } when child.source == s.source ->
              (not s.single) && self#can_reselect ~reselect effective_source
          | _ -> not (List.memq s excluded_sources)
      in
      try
        Some
          (pick_selection
             (find ~strict:all_predicates
                (fun (d, s) -> satisfied d && may_select s && s.source#is_ready)
                children))
      with Not_found -> None

    method fallible =
      not
        (List.exists
           (fun (d, s) ->
             (not s.source#fallible) && (not s.single) && trivially_true d)
           children)

    method private prepare_new_source ~child source =
      let new_source = new insert_initial_track_mark source in
      Typing.(new_source#frame_type <: self#frame_type);
      match (source#last_metadata, child.replay_meta) with
        | Some (_, m), true ->
            self#log#info "Replaying metadata for %s." source#id;
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
              | Some old_selection, None ->
                  ignore
                    (Lang.apply old_selection.child.on_leave
                       [("", Lang.source old_selection.child.source)]);
                  self#exchange_selected None
              | None, Some (predicate, c) ->
                  self#log#important "Switch to %s." c.source#id;
                  let new_source = self#prepare_new_source ~child:c c.source in
                  self#set_selected ~predicate ~child:c new_source
              | Some old_selection, Some (_, c)
                when old_selection.child.source == c.source ->
                  ()
              | Some old_selection, Some (predicate, c) ->
                  self#log#important "Switch to %s with transition." c.source#id;
                  ignore
                    (Lang.apply old_selection.child.on_leave
                       [("", Lang.source old_selection.child.source)]);
                  let new_source = self#prepare_new_source ~child:c c.source in
                  let s =
                    Lang.to_source
                      (Lang.apply c.on_select [("", Lang.source new_source)])
                  in
                  Typing.(s#frame_type <: self#frame_type);
                  self#set_selected ~predicate ~child:c s
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

let default_on_select =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (x) -> x"

let default_on_leave =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_) -> ()"

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
      ( "all_predicates",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Always evaluate all predicates when re-selecting." );
      ( "",
        (let transition_t =
           Lang.fun_t
             [(false, "", Lang.source_t return_t)]
             (Lang.source_t return_t)
         in
         let on_leave_t =
           Lang.fun_t [(false, "", Lang.source_t return_t)] Lang.unit_t
         in
         Lang.list_t
           (Lang.product_t pred_t
              (Lang.optional_method_t (Lang.source_t return_t)
                 [
                   ( "on_leave",
                     ([], on_leave_t),
                     "Called when switching away from this source. Defaults to \
                      `fun (_) -> ()`." );
                   ( "replay_metadata",
                     ([], Lang.bool_t),
                     "Replay metadata when switching to this source. Defaults \
                      to `true`." );
                   ( "single",
                     ([], Lang.bool_t),
                     "Forbid the selection of this source for two consecutive \
                      tracks. Defaults to `false`." );
                   ( "on_select",
                     ([], transition_t),
                     "Function called when selecting this source. It receives \
                      the source as argument and should return a source, \
                      allowing optional processing such as a fade-in or an \
                      intro jingle. Defaults to `fun (x) -> x`." );
                 ]))),
        None,
        Some "Sources with the predicate telling when they can be played." );
    ]
    ~return_t
    (fun p ->
      let source_method_opt name s_val =
        Value.Methods.find_opt name (Value.methods s_val)
      in
      let children =
        List.map
          (fun p ->
            let pred, s_val = Lang.to_product p in
            let source = Lang.to_source s_val in
            let on_leave =
              match source_method_opt "on_leave" s_val with
                | Some v -> v
                | None -> default_on_leave
            in
            let replay_meta =
              match source_method_opt "replay_metadata" s_val with
                | Some v -> Lang.to_bool v
                | None -> true
            in
            let single =
              match source_method_opt "single" s_val with
                | Some v -> Lang.to_bool v
                | None -> false
            in
            let on_select =
              match source_method_opt "on_select" s_val with
                | Some v -> v
                | None -> default_on_select
            in
            (pred, { source; on_select; on_leave; replay_meta; single }))
          (Lang.to_list (List.assoc "" p))
      in
      let ts = Lang.to_bool_getter (List.assoc "track_sensitive" p) in
      let all_predicates = Lang.to_bool (List.assoc "all_predicates" p) in
      new switch ~all_predicates ~track_sensitive:ts children)
