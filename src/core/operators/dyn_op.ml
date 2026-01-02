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
    inherit Source.source ~name:"source.dynamic" ()
    inherit Source.generate_from_multiple_sources ~merge ~track_sensitive ()
    method fallible = not infallible
    val mutable activation = []

    val current_source : (Clock.activation * Source.source) option Atomic.t =
      Atomic.make None

    method current_source = Option.map snd (Atomic.get current_source)

    initializer
      self#on_wake_up (fun () ->
          match (Atomic.get current_source, init) with
            | None, Some s -> ignore (self#switch s)
            | _ -> ())

    method private no_source id =
      if infallible then
        Lang.raise_error ~pos:[]
          ~message:
            (Printf.sprintf
               "Infallible source.dynamic %s was not able to prepare %s in \
                time! Make sure to either define infallible sources in the \
                source's dynamic function or mark the source as fallible.."
               self#id
               (match id with
                 | None -> "a source"
                 | Some id -> Printf.sprintf "source %s" id))
          "failure";
      None

    method prepare s =
      Typing.(s#frame_type <: self#frame_type);
      Clock.unify ~pos:self#pos s#clock self#clock;
      s#wake_up (self :> Clock.source)

    method private switch s =
      self#log#info "Switching to source %s" s#id;
      let a = self#prepare s in
      Atomic.set current_source (Some (a, s));
      if s#is_ready then Some s else self#no_source (Some s#id)

    method private exchange s =
      match Atomic.get current_source with
        | Some (_, s') when s == s' -> Some s
        | Some (a, s') ->
            let ret = self#switch s in
            s'#sleep a;
            ret
        | None -> self#switch s

    method private get_next reselect =
      self#mutexify
        (fun () ->
          let s =
            Lang.apply next_fn [] |> Lang.to_option |> Option.map Lang.to_source
          in
          match (s, self#current_source) with
            | None, Some s
              when self#can_reselect
                     ~reselect:(match reselect with `Force -> `Ok | v -> v)
                     s ->
                Some s
            | Some s, Some s' when s == s' ->
                if
                  self#can_reselect
                    ~reselect:(match reselect with `Force -> `Ok | v -> v)
                    s
                then Some s
                else self#no_source None
            | Some s, _ -> self#exchange s
            | _ -> self#no_source None)
        ()

    method private get_source ~reselect () =
      match (self#current_source, reselect) with
        | None, _ | _, `Force | Some _, `After_position _ ->
            self#get_next reselect
        | Some s, _ when self#can_reselect ~reselect s -> Some s
        | _ -> self#get_next reselect

    initializer
      self#on_wake_up (fun () -> ignore (self#get_source ~reselect:`Force ()));
      self#on_sleep (fun () ->
          match Atomic.exchange current_source None with
            | Some (a, s) -> s#sleep a
            | None -> ())

    method remaining =
      match self#current_source with Some s -> s#remaining | None -> -1

    method abort_track =
      match self#current_source with Some s -> s#abort_track | None -> ()

    method effective_source =
      match self#current_source with
        | Some s -> s#effective_source
        | None -> (self :> Source.source)

    method self_sync =
      match self_sync with
        | Some v -> (`Static, self#source_sync v)
        | None ->
            ( `Dynamic,
              match self#current_source with
                | Some s -> snd s#self_sync
                | None -> None )
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
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
        Lang.fun_t [] (Lang.nullable_t (Lang.source_t frame_t)),
        None,
        Some
          "Function returning the source to be used, `null` means keep current \
           source." );
    ]
    ~return_t:frame_t
    ~descr:
      "Dynamically change the underlying source: it can either be changed by \
       the function given as argument, which returns the source to be played, \
       or by calling the `set` method."
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
            ([], Lang.fun_t [(false, "", Lang.source_t frame_t)] Lang.unit_t);
          descr = "Prepare a source that will be returned later.";
          value =
            (fun s ->
              Lang.val_fun
                [("", "x", None)]
                (fun p ->
                  let child = List.assoc "x" p |> Lang.to_source in
                  Typing.(child#frame_type <: s#frame_type);
                  Clock.unify ~pos:s#pos child#clock s#clock;
                  child#get_up (Printf.sprintf "%s.prepare" s#id);
                  Lang.unit));
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
