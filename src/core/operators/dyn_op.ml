(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

    method fallible = not infallible
    val mutable activation = []
    val source : Source.source option Atomic.t = Atomic.make init
    val mutable last_select = Unix.gettimeofday ()
    val proposed = Atomic.make None
    method propose s = Atomic.set proposed (Some s)

    method private no_source =
      if infallible then
        Lang.raise_error ~pos:[]
          ~message:
            (Printf.sprintf
               "Infallible source.dynamic %s was not able to prepare a source \
                in time! Make sure to eithe define infallible sources in the \
                source's dynamic function or mark the source as fallible.."
               self#id)
          "failure";
      None

    method private prepare s =
      Typing.(s#frame_type <: self#frame_type);
      Clock.unify ~pos:self#pos s#clock self#clock;
      s#wake_up;
      (match Atomic.exchange source (Some s) with
        | Some s -> s#sleep
        | None -> ());
      if s#is_ready then Some s else self#no_source

    method private get_next reselect =
      self#mutexify
        (fun () ->
          match Atomic.exchange proposed None with
            | Some s -> self#prepare s
            | None -> (
                last_select <- Unix.gettimeofday ();
                let s =
                  Lang.apply f [] |> Lang.to_option |> Option.map Lang.to_source
                in
                match s with
                  | None -> (
                      match Atomic.get source with
                        | Some s
                          when self#can_reselect
                                 ~reselect:
                                   (match reselect with
                                     | `Force -> `Ok
                                     | v -> v)
                                 s ->
                            Some s
                        | _ -> self#no_source)
                  | Some s -> self#prepare s))
        ()

    method private get_source ~reselect () =
      match (Atomic.get source, reselect) with
        | None, _ | _, `Force | Some _, `After_position _ ->
            self#get_next reselect
        | Some s, _ when self#can_reselect ~reselect s -> Some s
        | Some _, _ when Unix.gettimeofday () -. last_select < resurection_time
          ->
            None
        | _ -> self#get_next reselect

    initializer
      self#on_wake_up (fun () ->
          Lang.iter_sources
            (fun s -> Typing.(s#frame_type <: self#frame_type))
            f;
          ignore (self#get_source ~reselect:`Force ()));
      self#on_sleep (fun () ->
          match Atomic.exchange source None with
            | Some s -> s#sleep
            | None -> ())

    method remaining =
      match Atomic.get source with Some s -> s#remaining | None -> -1

    method abort_track =
      match Atomic.get source with Some s -> s#abort_track | None -> ()

    method seek_source =
      match Atomic.get source with
        | Some s -> s#seek_source
        | None -> (self :> Source.source)

    method self_sync =
      match self_sync with
        | Some v -> (`Static, self#source_sync v)
        | None -> (
            ( `Dynamic,
              match Atomic.get source with
                | Some s -> snd s#self_sync
                | None -> None ))
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
