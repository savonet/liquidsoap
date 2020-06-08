(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

class dynamic ~kind ~delay ~infallible ~track_sensitive f =
  object (self)
    inherit Source.source ~name:"source.dynamic" kind

    method stype = if infallible then Source.Infallible else Source.Fallible

    val mutable activation = []

    (* The dynamic stuff: #select calls the selection function and changes the
       source when needed, and #unregister_source does what its name says. Any
       sequence of calls to #select and #unregister_source is okay but they
       should not overlap. All that matters for cleanliness is that
       #unregister_source comes last, which #sleep ensures. *)
    val source_lock = Mutex.create ()

    val mutable source : Source.source option = None

    method private unregister_source ~already_locked =
      let unregister () =
        match source with
          | Some s ->
              s#leave (self :> Source.source);
              source <- None
          | None -> ()
      in
      if already_locked then unregister ()
      else Tutils.mutexify source_lock unregister ()

    method private select =
      (* Avoid that a new source gets assigned to the default clock. *)
      Clock.collect_after
        (Tutils.mutexify source_lock (fun () ->
             let s = Lang.to_source (Lang.apply f []) in
             match source with
               | Some s' when s' == s -> ()
               | _ ->
                   Source.Kind.unify s#kind_var self#kind_var;
                   Clock.unify s#clock self#clock;
                   s#get_ready activation;
                   self#unregister_source ~already_locked:true;
                   source <- Some s))

    (* Source methods: attempt to #select as soon as it could be useful for the
       selection function to change the source. *)
    method private wake_up ancestors =
      activation <- (self :> Source.source) :: ancestors;
      Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f;
      self#select

    method private sleep =
      Lang.iter_sources
        (fun s -> s#leave ~dynamic:true (self :> Source.source))
        f;
      self#unregister_source ~already_locked:false

    method is_ready =
      (* TODO: do we want to select on each is_ready call? Or only when
         s#is_ready is false? *)
      self#select;
      match source with Some s when s#is_ready -> true | _ -> false

    (** Produced stream (in master ticks) since last selection. *)
    val mutable produced = 0

    method private get_frame frame =
      let pos = Frame.position frame in
      begin
        match source with
        | Some s -> s#get frame
        | None -> Frame.add_break frame (Frame.position frame)
      end;
      produced <- produced + (Frame.position frame - pos);
      (* Reselect on each track or when not track sensitive and enough time has
         elapsed. *)
      if
        Frame.is_partial frame
        || ((not (track_sensitive ())) && produced > delay)
      then (
        self#select;
        produced <- 0 )

    method remaining = match source with Some s -> s#remaining | None -> -1

    method abort_track =
      match source with Some s -> s#abort_track | None -> ()

    method seek n = match source with Some s -> s#seek n | None -> 0

    method self_sync = match source with Some s -> s#self_sync | None -> false
  end

let () =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "source.dynamic"
    [
      ( "delay",
        Lang.float_t,
        Some (Lang.float 0.5),
        Some
          "Minimum delay (in seconds) before re-selecting a source (if not \
           track-sensitive)." );
      ( "infallible",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Whether the source is infallible or not (be careful when setting \
           this, it will not be checked by the typing system)." );
      ( "track_sensitive",
        Lang.bool_getter_t (),
        Some (Lang.bool true),
        Some "Only change source on track boundaries." );
      ("", Lang.fun_t [] (Lang.source_t k), None, None);
    ]
    ~return_t:k ~descr:"Dynamically change the underlying source."
    ~category:Lang.TrackProcessing ~flags:[Lang.Experimental]
    (fun p ->
      let delay =
        Frame.master_of_seconds (Lang.to_float (List.assoc "delay" p))
      in
      let infallible = Lang.to_bool (List.assoc "infallible" p) in
      let track_sensitive =
        Lang.to_bool_getter (List.assoc "track_sensitive" p)
      in
      new dynamic ~kind ~delay ~infallible ~track_sensitive (List.assoc "" p))
