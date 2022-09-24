(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

class dyn ~init ~track_sensitive ~infallible ~resurection_time f =
  object (self)
    inherit Source.source ~name:"source.dynamic" ()
    method stype = if infallible then `Infallible else `Fallible
    val mutable activation = []

    (* The dynamic stuff: #select calls the selection function and changes
     * the source when needed, and #unregister_source does what its name says.
     * Any sequence of calls to #select and #unregister_source is okay
     * but they should not overlap.
     * All that matters for cleanliness is that #unregister_source comes
     * last, which #sleep ensures. *)
    val source_lock = Mutex.create ()
    val mutable source : Source.source option = init

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

    val mutable last_select = 0.

    (* Proposed source for next round. *)
    val mutable proposal = None
    method propose s = proposal <- Some s

    method private select =
      (* Avoid that a new source gets assigned to the default clock. *)
      Clock.collect_after
        (Tutils.mutexify source_lock (fun () ->
             let s =
               match proposal with
                 | Some s ->
                     proposal <- None;
                     Some s
                 | None ->
                     Lang.apply f [] |> Lang.to_option
                     |> Option.map Lang.to_source
             in
             if track_sensitive then last_select <- Unix.gettimeofday ();
             match s with
               | None -> ()
               | Some s ->
                   Typing.(s#frame_type <: self#frame_type);
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
      if (not track_sensitive) || source = None then self#select;
      match source with
        | Some s when s#is_ready -> true
        | _ ->
            if
              track_sensitive && resurection_time <> None
              && Unix.gettimeofday () -. last_select
                 >= Option.get resurection_time
            then self#select;
            false

    method private get_frame frame =
      begin
        match source with
        | Some s -> s#get frame
        | None -> Frame.add_break frame (Frame.position frame)
      end;
      if (not track_sensitive) || Frame.is_partial frame then self#select

    method remaining = match source with Some s -> s#remaining | None -> -1

    method abort_track =
      match source with Some s -> s#abort_track | None -> ()

    method seek n = match source with Some s -> s#seek n | None -> 0

    method self_sync =
      (`Dynamic, match source with Some s -> snd s#self_sync | None -> false)
  end

let () =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "source.dynamic"
    [
      ( "init",
        Lang.nullable_t (Lang.source_t frame_t),
        Some Lang.null,
        Some "Initial value for the source" );
      ( "track_sensitive",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Whether the source should only be updated on track change." );
      ( "infallible",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Whether the source is infallible or not (be careful when setting \
           this, it will not be checked by the typing system)." );
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
            Lang.val_fun [("", "x", None)] (fun p ->
                s#propose (List.assoc "x" p |> Lang.to_source);
                Lang.unit) );
      ]
    (fun p ->
      let init =
        List.assoc "init" p |> Lang.to_option |> Option.map Lang.to_source
      in
      let track_sensitive = List.assoc "track_sensitive" p |> Lang.to_bool in
      let infallible = List.assoc "infallible" p |> Lang.to_bool in
      let resurection_time =
        List.assoc "resurection_time" p |> Lang.to_valued_option Lang.to_float
      in
      new dyn
        ~init ~track_sensitive ~infallible ~resurection_time (List.assoc "" p))
