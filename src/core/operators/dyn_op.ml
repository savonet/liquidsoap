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

class dyn ~init ~track_sensitive ~infallible ~resurection_time ~self_sync f =
  object (self)
    inherit Source.source ~name:"source.dynamic" ()

    inherit
      Source.generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive ()

    method stype = if infallible then `Infallible else `Fallible
    val mutable activation = []
    val mutable source : Source.source option = init

    method private unregister_source ~already_locked =
      let unregister () =
        match source with
          | Some s ->
              s#leave (self :> Source.source);
              source <- None
          | None -> ()
      in
      if already_locked then unregister () else self#mutexify unregister ()

    val mutable last_select = Unix.gettimeofday ()
    val mutable proposed = None
    method propose s = proposed <- Some s

    method private get_source ~reselect () =
      (* Avoid that a new source gets assigned to the default clock. *)
      Clock.collect_after
        (self#mutexify (fun () ->
             let next () =
               last_select <- Unix.gettimeofday ();
               let s =
                 Lang.apply f [] |> Lang.to_option |> Option.map Lang.to_source
               in
               match s with
                 | None -> None
                 | Some s ->
                     Typing.(s#frame_type <: self#frame_type);
                     Clock.unify ~pos:self#pos s#clock self#clock;
                     s#get_ready activation;
                     self#unregister_source ~already_locked:true;
                     source <- Some s;
                     source
             in
             match (source, proposed) with
               | Some s, _ when s#is_ready && not reselect -> source
               | _, Some s when s#is_ready ->
                   source <- Some s;
                   source
               | Some s, _
                 when (not reselect) && (not s#is_ready)
                      && Unix.gettimeofday () -. last_select < resurection_time
                 ->
                   next ()
               | _ -> next ()))

    (* Source methods: attempt to #get_source as soon as it could be useful for the
       selection function to change the source. *)
    method! private wake_up ancestors =
      activation <- (self :> Source.source) :: ancestors;
      Lang.iter_sources
        (fun s ->
          Typing.(s#frame_type <: self#frame_type);
          s#get_ready activation)
        f;
      ignore (self#get_source ~reselect:true ())

    method! private sleep =
      Lang.iter_sources (fun s -> s#leave (self :> Source.source)) f;
      self#unregister_source ~already_locked:false

    method remaining = match source with Some s -> s#remaining | None -> -1

    method abort_track =
      match source with Some s -> s#abort_track | None -> ()

    method seek_source =
      match source with
        | Some s -> s#seek_source
        | None -> (self :> Source.source)

    method self_sync =
      match self_sync with
        | Some v -> (`Static, v)
        | None -> (
            ( `Dynamic,
              match source with Some s -> snd s#self_sync | None -> false ))
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
      ( "resurection_time",
        Lang.nullable_t Lang.float_t,
        Some (Lang.float 1.),
        Some
          "When track sensitive and the source is unavailable, how long we \
           should wait before trying to update source again (`null` means \
           never)." );
      ( "",
        Lang.fun_t [] (Lang.nullable_t (Lang.source_t frame_t)),
        Some (Lang.val_fun [] (fun _ -> Lang.null)),
        Some
          "Function returning the source to be used, `null` means keep current \
           source." );
    ]
    ~return_t:frame_t
    ~descr:
      "Dynamically change the underlying source: it can either be changed by \
       the function given as argument, which returns the source to be played, \
       or by calling the `set` method."
    ~category:`Track ~flags:[`Experimental]
    ~meth:
      [
        ( "set",
          ([], Lang.fun_t [(false, "", Lang.source_t frame_t)] Lang.unit_t),
          "Set the source.",
          fun s ->
            Lang.val_fun
              [("", "x", None)]
              (fun p ->
                s#propose (List.assoc "x" p |> Lang.to_source);
                Lang.unit) );
      ]
    (fun p ->
      let init =
        List.assoc "init" p |> Lang.to_option |> Option.map Lang.to_source
      in
      let track_sensitive = List.assoc "track_sensitive" p |> Lang.to_getter in
      let track_sensitive () = Lang.to_bool (track_sensitive ()) in
      let infallible = List.assoc "infallible" p |> Lang.to_bool in
      let resurection_time =
        List.assoc "resurection_time" p |> Lang.to_valued_option Lang.to_float
      in
      let resurection_time = Option.value ~default:(-1.) resurection_time in
      let self_sync =
        Lang.to_valued_option Lang.to_bool (List.assoc "self_sync" p)
      in
      let next = List.assoc "" p in
      new dyn
        ~init ~track_sensitive ~infallible ~resurection_time ~self_sync next)
