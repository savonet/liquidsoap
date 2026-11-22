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

class dyn ~init ~track_sensitive ~infallible ~self_sync ~merge next_fn =
  object (self)
    inherit Source.source ~name:"source.dynamic" () as super
    inherit Source.generate_from_multiple_sources ()
    method private next_entry = if merge () then `Continue else `Track
    method fallible = not infallible

    method prepare s =
      Typing.(s#frame_type <: self#frame_type);
      Clock.unify ~pos:self#pos s#clock self#clock;
      s#wake_up (self :> Clock.source)

    (* The source being played, with the activation that keeps it awake. *)
    val current = Atomic.make None
    method current_source = Option.map snd (Atomic.get current)

    (* A source the function designated and that is not played yet. *)
    val mutable pending = None

    method private release held =
      Option.iter (fun (activation, s) -> s#sleep activation) held

    (* Keeps [s] as the source to play next. A source we already hold is not
       woken again: an activation that came with it is one too many. *)
    method private designate ~activation s =
      let holds = function Some (_, held) -> held == s | None -> false in
      if holds (Atomic.get current) then (
        (* The function went back to what plays: whatever it designated
           since is dropped. *)
        Option.iter s#sleep activation;
        self#release pending;
        pending <- None)
      else if holds pending then Option.iter s#sleep activation
      else (
        self#log#info "Preparing source %s" s#id;
        let activation =
          match activation with Some a -> a | None -> self#prepare s
        in
        self#release pending;
        pending <- Some (activation, s))

    initializer
      self#on_wake_up (fun () ->
          match (self#current_source, init) with
            | None, Some s -> self#designate ~activation:None s
            | _ -> ());
      self#on_sleep (fun () ->
          self#release (Atomic.exchange current None);
          self#release pending;
          pending <- None)

    (* Asks the function what to play. This runs script code and may wake a
       source, but enters nothing: the function can be asked again without
       its answer having been played. *)
    method private consult () =
      self#mutable_lock
        (fun () ->
          (match Lang.to_option (Lang.apply next_fn []) with
            | None -> ()
            | Some v ->
                let meth, v = Lang.split_meths v in
                let activation =
                  Option.map Lang_source.Activation_val.of_value
                    (List.assoc_opt "activation" meth)
                in
                self#designate ~activation (Lang.to_source v));
          match (pending, self#current_source) with
            | Some (_, s), _ when s#is_ready -> `Pending
            | Some _, _ -> `Nothing
            | None, Some s when s#is_ready -> `Current
            | None, _ -> `Nothing)
        ()

    (* What a probe decided: acted on when the frame is read. *)
    val mutable decided = `Nothing

    method private choose_next_source ~boundary =
      decided <-
        (match self#current_source with
          | Some s when (not boundary) && track_sensitive () && s#is_ready ->
              `Current
          | _ -> self#consult ());

      (* Declared infallible, we answer ready whatever we found: reading us
         with nothing to deliver is the failure, and it is raised there. *)
      infallible || decided <> `Nothing

    method private enter_chosen_source =
      match decided with
        | `Current -> self#current_source
        | `Pending ->
            let entered = Option.get pending in
            self#log#info "Switching to source %s" (snd entered)#id;
            pending <- None;

            (* The source entered is awake before the one left is released. *)
            self#release (Atomic.exchange current (Some entered));
            decided <- `Current;
            Some (snd entered)
        | `Nothing ->
            if infallible then
              Lang.raise_error ~pos:[]
                ~message:
                  (Printf.sprintf
                     "Infallible source.dynamic %s was not able to prepare a \
                      source in time! Make sure to either define infallible \
                      sources in the source's dynamic function or mark the \
                      source as fallible.."
                     self#id)
                "failure";
            None

    method remaining =
      match self#current_source with Some s -> s#remaining | None -> -1

    method abort_track =
      self#end_track;
      match self#current_source with Some s -> s#abort_track | None -> ()

    method effective_source =
      match self#current_source with
        | Some s -> s#effective_source
        | None -> (self :> Source.source)

    (* With nothing played yet, we compose like the source we are about to
       play. Before the function was ever asked there is none: we then wait
       for track boundaries, which costs a cycle if the source turns out to
       be live, where cutting in would cost a track if it is not. *)
    method! resolved_composition =
      match (self#composition, self#current_source, pending) with
        | `Passthrough, None, Some (_, s) -> s#resolved_composition
        | `Passthrough, None, None -> `File
        | _ -> super#resolved_composition

    method private self_sync =
      match self_sync with
        | Some v -> (`Static, self#source_sync v)
        | None ->
            ( `Dynamic,
              match self#current_source with
                | Some s -> snd s#cached_self_sync
                | None -> None )
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let source_t =
    Lang.optional_method_t (Lang.source_t frame_t)
      [
        ( "activation",
          ([], Lang_source.Activation_val.t),
          "Optional activation if the source has been previously activated for \
           this dynamic source" );
      ]
  in
  Lang.add_operator ~base:Muxer.source "dynamic"
    [
      ( "init",
        Lang.nullable_t (Lang.source_t frame_t),
        Some Lang.null,
        Some "Initial value for the source" );
      ( "track_sensitive",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Whether the source should only be updated on track change." );
      ( "infallible",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Whether the source is infallible or not (be careful when setting \
           this, it will not be checked by the typing system)." );
      ( "self_sync",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some "For the source's `self_sync` property." );
      ( "merge",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Set or return `true` to merge subsequent tracks." );
      ( "",
        Lang.fun_t [] (Lang.nullable_t source_t),
        None,
        Some
          "Function returning the source to be used, `null` means keep current \
           source." );
    ]
    ~return_t:frame_t
    ~descr:
      "Dynamically change the underlying source: the function given as \
       argument is called to get the source to play, and returns `null` to \
       keep the current one."
    ~category:`Track
    ~meth:
      [
        {
          name = "current_source";
          scheme = ([], Lang.fun_t [] (Lang.nullable_t (Lang.source_t frame_t)));
          descr = "Return the source currently selected.";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ ->
                  match s#current_source with
                    | None -> Lang.null
                    | Some s -> Lang.source s));
        };
        {
          name = "prepare";
          scheme =
            ( [],
              Lang.fun_t
                [(false, "", Lang.source_t frame_t)]
                (Lang.method_t (Lang.source_t frame_t)
                   [
                     ( "activation",
                       ([], Lang_source.Activation_val.t),
                       "Source activation" );
                   ]) );
          descr =
            "Prepare a source that will be returned later. Returned source has \
             new activation attached to it and should be used to submit to the \
             source later.";
          value =
            (fun s ->
              Lang.val_fun
                [("", "x", None)]
                (fun p ->
                  let v = List.assoc "x" p in
                  let child = Lang.to_source v in
                  let a = s#prepare child in
                  Lang.meth v
                    [("activation", Lang_source.Activation_val.to_value a)]));
        };
      ]
    (fun p ->
      let init =
        List.assoc "init" p |> Lang.to_option |> Option.map Lang.to_source
      in
      let track_sensitive = List.assoc "track_sensitive" p |> Lang.to_getter in
      let track_sensitive () = Lang.to_bool (track_sensitive ()) in
      let infallible = List.assoc "infallible" p |> Lang.to_bool in
      let merge = Lang.to_getter (List.assoc "merge" p) in
      let merge () = Lang.to_bool (merge ()) in
      let self_sync =
        Lang.to_valued_option Lang.to_bool (List.assoc "self_sync" p)
      in
      let next = List.assoc "" p in
      new dyn ~init ~track_sensitive ~infallible ~merge ~self_sync next)
