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

type child = {
  source : source;
  on_select : source option -> source -> source;
  on_leave : source -> bool -> unit;
  track_sensitive : unit -> bool;
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

class switch ~all_predicates children =
  let cases = List.map (fun (_, s) -> s) children in
  let sources = List.map (fun c -> c.source) cases in
  let self_sync_type = Clock_base.self_sync_type sources in
  (* Track-sensitivity is determined per-source from the selected child.
     We use a ref so it can be captured by the generate_from_multiple_sources
     mixin at construction time and updated in set_selected. *)
  let track_sensitive = ref (fun () -> true) in
  object (self)
    inherit operator ~name:"switch" (List.map (fun x -> x.source) cases)

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive:(fun () -> !track_sensitive ())
        ()

    (* Ensure track_sensitive ref is reset when no source is selected. *)
    initializer self#on_sleep (fun () -> track_sensitive := fun () -> true)
    val selected : selection option Atomic.t = Atomic.make None
    method selected = Atomic.get selected

    method exchange_selected v =
      match Atomic.exchange selected v with
        | Some { sleep } -> sleep ()
        | _ -> ()

    method set_selected ~predicate ~child effective_source =
      track_sensitive := child.track_sensitive;
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

    method private prepare_new_source source =
      let new_source = new insert_initial_track_mark source in
      Typing.(new_source#frame_type <: self#frame_type);
      new_source

    (* [incoming] is the child being selected next, if any. Its
       track_sensitive setting determines whether the leaving source was
       preempted mid-track (track_sensitive=false) or left at a natural track
       boundary (track_sensitive=true). When there is no incoming source the
       leave is unconditional and we pass true (no skip needed). *)
    method private apply_on_leave ~incoming child =
      let ts =
        match incoming with Some c -> c.track_sensitive () | None -> true
      in
      child.on_leave child.source ts

    method private apply_on_select ~ending ~starting child =
      let s = child.on_select ending starting in
      Typing.(s#frame_type <: self#frame_type);
      s

    method get_source ~reselect () =
      match self#selected with
        | Some s
          when (s.child.track_sensitive () || satisfied s.predicate)
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
                  self#apply_on_leave ~incoming:None old_selection.child;
                  self#exchange_selected None
              | None, Some (predicate, c) ->
                  self#log#important "Switch to %s." c.source#id;
                  let new_source = self#prepare_new_source c.source in
                  let s =
                    self#apply_on_select ~ending:None ~starting:new_source c
                  in
                  self#set_selected ~predicate ~child:c s
              | Some old_selection, Some (_, c)
                when old_selection.child.source == c.source ->
                  ()
              | Some old_selection, Some (predicate, c) ->
                  self#log#important "Switch to %s with transition." c.source#id;
                  self#apply_on_leave ~incoming:(Some c) old_selection.child;
                  let new_source = self#prepare_new_source c.source in
                  let s =
                    self#apply_on_select
                      ~ending:(Some old_selection.child.source)
                      ~starting:new_source c
                  in
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
            Type.meth ~optional:true "ending" ([], Lang.source_t return_t)
            @@ Lang.record_t [("starting", Lang.source_t return_t)] );
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
    Type.meth ~optional:true "single" ([], Lang.bool_t)
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
            let on_leave =
              match find_opt "on_leave" s_val with
                | Some on_leave ->
                    fun s ts ->
                      let record =
                        Lang.record
                          [
                            ("source", Lang.source s);
                            ("track_sensitive", Lang.bool ts);
                          ]
                      in
                      ignore (Lang.apply on_leave [("", record)])
                | None -> fun _s _ts -> ()
            in
            let on_select =
              match find_opt "on_select" s_val with
                | Some on_select ->
                    fun ending starting ->
                      let record =
                        Lang.record
                          ((match ending with
                             | None -> []
                             | Some s -> [("ending", Lang.source s)])
                          @ [("starting", Lang.source starting)])
                      in
                      Lang.to_source (Lang.apply on_select [("", record)])
                | None -> fun _ending starting -> starting
            in
            (pred, { source; on_select; on_leave; track_sensitive; single }))
          (Lang.to_list (List.assoc "" p))
      in
      let all_predicates = Lang.to_bool (List.assoc "all_predicates" p) in
      new switch ~all_predicates children)
